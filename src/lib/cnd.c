#include "rlang.h"


static sexp* x_sym = NULL;
static sexp* msg_call = NULL;
static sexp* wng_call = NULL;
static sexp* cnd_signal_call = NULL;
static sexp* wng_signal_call = NULL;
static sexp* err_signal_call = NULL;


#define BUFSIZE 8192

#define INTERP(BUF, FMT, DOTS)                  \
  {                                             \
    va_list dots;                               \
    va_start(dots, FMT);                        \
    vsnprintf(BUF, BUFSIZE, FMT, dots);         \
    va_end(dots);                               \
                                                \
    BUF[BUFSIZE - 1] = '\0';                    \
  }

void r_inform(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  sexp* env = KEEP(r_new_environment(r_base_env, 1));
  r_env_poke(env, x_sym, KEEP(r_chr(buf)));
  r_eval(msg_call, env);

  FREE(2);
}
void r_warn(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  sexp* env = KEEP(r_new_environment(r_base_env, 1));
  r_env_poke(env, x_sym, KEEP(r_chr(buf)));
  r_eval(wng_call, env);

  FREE(2);
}
void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  Rf_errorcall(r_null, buf);

  while (1); // No return
}
sexp* r_interp_str(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  return r_chr(buf);
}

static void signal_retirement(const char* source, const char* buf);
static sexp* deprecated_env = NULL;

void r_warn_deprecated(const char* id, const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  id = id ? id : buf;
  sexp* id_ = r_sym(id);

  // Warn once per session
  if (r_env_has(deprecated_env, id_)) {
    return;
  }
  r_env_poke(deprecated_env, id_, r_shared_true);


  const char* note;
  if (r_has_colour()) {
    note = "\n\033[90mThis warning is displayed once per session.\033[39m";
  } else {
    note = "\nThis warning is displayed once per session.";
  }

  if (strlen(buf) + strlen(note) + 1 < BUFSIZE) {
    strcat(buf, note);
  }

  signal_retirement(".Deprecated(msg = x)", buf);
}

void r_abort_defunct(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  signal_retirement(".Defunct(msg = x)", buf);

  r_abort("Internal error: Unexpected return after `.Defunct()`");
}

static void signal_retirement(const char* source, const char* buf) {
  sexp* call = KEEP(r_parse(source));

  sexp* env = KEEP(r_new_environment(r_base_env, 1));
  r_env_poke(env, x_sym, KEEP(r_chr(buf)));

  r_eval(call, env);
  FREE(3);
}

static sexp* new_condition_names(sexp* data) {
  if (!r_is_named(data)) {
    r_abort("Conditions must have named data fields");
  }

  sexp* data_nms = r_vec_names(data);

  if (r_chr_has_any(data_nms, (const char* []) { "message", "call", NULL })) {
    r_abort("Conditions can't have `message` or `call` data fields");
  }

  sexp* nms = KEEP(r_new_vector(r_type_character, r_length(data) + 2));
  r_chr_poke(nms, 0, r_string("message"));
  r_chr_poke(nms, 1, r_string("call"));
  r_vec_poke_n(nms, 2, data_nms, 0, r_length(nms) - 2);

  FREE(1);
  return nms;
}
sexp* r_new_condition(sexp* subclass, sexp* msg, sexp* call, sexp* data) {
  if (msg == r_null) {
    msg = r_shared_empty_chr;
  } else if (!r_is_scalar_character(msg)) {
    r_abort("Condition message must be a string");
  }

  r_ssize n_data = r_length(data);
  sexp* cnd = KEEP(r_new_vector(VECSXP, n_data + 2));

  r_list_poke(cnd, 0, msg);
  r_list_poke(cnd, 1, call);
  r_vec_poke_n(cnd, 2, data, 0, r_length(cnd) - 2);

  r_poke_names(cnd, KEEP(new_condition_names(data)));
  r_poke_class(cnd, KEEP(chr_append(subclass, KEEP(r_string("condition")))));

  FREE(4);
  return cnd;
}


void r_cnd_signal(sexp* cnd) {
  sexp* call = r_null;

  switch (r_cnd_type(cnd)) {
  case r_cnd_type_message:
    call = msg_call;
    break;
  case r_cnd_type_warning:
    call = wng_signal_call;
    break;
  case r_cnd_type_error:
    call = err_signal_call;
    break;
  case r_cnd_type_interrupt:
    r_interrupt();
    return;
  default:
    call = cnd_signal_call;
    break;
  }

  sexp* env = KEEP(r_new_environment(r_base_env, 1));
  r_env_poke(env, x_sym, cnd);

  r_eval(call, env);
  FREE(1);
}


#ifdef RLANG_HAS_RINTERFACE_H
#include <Rinterface.h>
void r_interrupt() {
  Rf_onintr();
  r_abort("Internal error: Simulated interrupt not processed");
}
#else
#include <Rembedded.h>
void r_interrupt() {
  UserBreak = 1;
  R_CheckUserInterrupt();
  r_abort("Internal error: Simulated interrupt not processed");
}
#endif

enum r_condition_type r_cnd_type(sexp* cnd) {
  sexp* classes = r_get_class(cnd);
  if (r_typeof(cnd) != r_type_list ||
      r_typeof(classes) != r_type_character) {
    goto error;
  }

  r_ssize n_classes = r_length(classes);

  for (r_ssize i = 0; i < n_classes; ++i) {
    const char* class_str = r_str_deref(r_chr_get(classes, i));
    switch (class_str[0]) {
    case 'c':
      if (strcmp(class_str, "condition")) {
        continue;
      } else {
        return r_cnd_type_condition;
      }
    case 'm':
      if (strcmp(class_str, "message")) {
        continue;
      } else {
        return r_cnd_type_message;
      }
    case 'w':
      if (strcmp(class_str, "warning")) {
        continue;
      } else {
        return r_cnd_type_warning;
      }
    case 'e':
      if (strcmp(class_str, "error")) {
        continue;
      } else {
        return r_cnd_type_error;
      }
    case 'i':
      if (strcmp(class_str, "interrupt")) {
        continue;
      } else {
        return r_cnd_type_interrupt;
      }
    default:
      continue;
    }
  }

 error:
  r_abort("`cnd` is not a condition object");
}

sexp* rlang_ns_get(const char* name);

void r_init_library_cnd() {
  x_sym = r_sym("x");

  msg_call = r_parse("message(x)");
  r_mark_precious(msg_call);

  wng_call = r_parse("warning(x, call. = FALSE)");
  r_mark_precious(wng_call);

  wng_signal_call = r_parse("warning(x)");
  r_mark_precious(wng_signal_call);

  err_signal_call = r_parse("stop(x)");
  r_mark_precious(err_signal_call);

  const char* cnd_signal_source =
    "withRestarts(rlang_muffle = function() NULL, signalCondition(x))";
  cnd_signal_call = r_parse(cnd_signal_source);
  r_mark_precious(cnd_signal_call);

  deprecated_env = rlang_ns_get("deprecation_env");
}
