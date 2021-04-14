#include "rlang.h"

// From rlang/vec.c
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n);


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

__attribute__((noreturn))
void r_stop_internal(const char* fn, const char* fmt, ...) {
  R_CheckStack2(BUFSIZE);

  char msg[BUFSIZE];
  INTERP(msg, fmt, ...);

  r_abort("Internal error in `%s()`: %s", fn, msg);
}

static r_obj* msg_call = NULL;
void r_inform(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  r_eval_with_x(msg_call, KEEP(r_chr(buf)), r_base_env);

  FREE(1);
}

static r_obj* wng_call = NULL;
void r_warn(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  r_eval_with_x(wng_call, KEEP(r_chr(buf)), r_base_env);

  FREE(1);
}

static r_obj* err_call = NULL;
void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  r_eval_with_x(err_call, KEEP(r_chr(buf)), r_base_env);

  while (1); // No return
}

// From vec-chr.c
r_obj* chr_append(r_obj* chr, r_obj* r_string);

static r_obj* new_condition_names(r_obj* data) {
  if (!r_is_named(data)) {
    r_abort("Conditions must have named data fields");
  }

  r_obj* data_nms = r_names(data);

  if (r_chr_has_any(data_nms, (const char* []) { "message", NULL })) {
    r_abort("Conditions can't have a `message` data field");
  }

  r_obj* nms = KEEP(r_alloc_character(r_length(data) + 1));
  r_chr_poke(nms, 0, r_str("message"));
  r_vec_poke_n(nms, 1, data_nms, 0, r_length(nms) - 1);

  FREE(1);
  return nms;
}
r_obj* r_new_condition(r_obj* subclass, r_obj* msg, r_obj* data) {
  if (msg == r_null) {
    msg = r_chrs.empty_string;
  } else if (!r_is_string(msg)) {
    r_abort("Condition message must be a string");
  }

  r_ssize n_data = r_length(data);
  r_obj* cnd = KEEP(r_alloc_list(n_data + 1));

  r_list_poke(cnd, 0, msg);
  r_vec_poke_n(cnd, 1, data, 0, r_length(cnd) - 1);

  r_attrib_poke_names(cnd, KEEP(new_condition_names(data)));
  r_attrib_poke_class(cnd, KEEP(chr_append(subclass, KEEP(r_str("condition")))));

  FREE(4);
  return cnd;
}


static r_obj* cnd_signal_call = NULL;
static r_obj* wng_signal_call = NULL;
static r_obj* err_signal_call = NULL;

void r_cnd_signal(r_obj* cnd) {
  r_obj* call = r_null;

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

  r_eval_with_x(call, cnd, r_base_env);
}


#ifdef _WIN32
#include <Rembedded.h>
void r_interrupt() {
  UserBreak = 1;
  R_CheckUserInterrupt();
  r_abort("Internal error: Simulated interrupt not processed");
}
#else
#include <Rinterface.h>
void r_interrupt() {
  Rf_onintr();
  r_abort("Internal error: Simulated interrupt not processed");
}
#endif

enum r_condition_type r_cnd_type(r_obj* cnd) {
  r_obj* classes = r_class(cnd);
  if (r_typeof(cnd) != R_TYPE_list ||
      r_typeof(classes) != R_TYPE_character) {
    goto error;
  }

  r_ssize n_classes = r_length(classes);

  for (r_ssize i = 0; i < n_classes; ++i) {
    const char* class_str = r_str_c_string(r_chr_get(classes, i));
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

r_obj* rlang_ns_get(const char* name);

void r_init_library_cnd() {
  msg_call = r_parse("message(x)");
  r_preserve(msg_call);

  wng_call = r_parse("warning(x, call. = FALSE)");
  r_preserve(wng_call);

  err_call = r_parse("rlang::abort(x)");
  r_preserve(err_call);

  wng_signal_call = r_parse("warning(x)");
  r_preserve(wng_signal_call);

  err_signal_call = r_parse("rlang:::signal_abort(x)");
  r_preserve(err_signal_call);

  const char* cnd_signal_source =
    "withRestarts(rlang_muffle = function() NULL, signalCondition(x))";
  cnd_signal_call = r_parse(cnd_signal_source);
  r_preserve(cnd_signal_call);
}
