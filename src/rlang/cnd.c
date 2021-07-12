#include "rlang.h"
#include "decl/cnd-decl.h"


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
void (*r_stop_internal)(const char* fn, const char* fmt, ...) = NULL;

static r_obj* msg_call = NULL;
void r_inform(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  r_eval_with_x(msg_call, KEEP(r_chr(buf)), r_envs.ns);

  FREE(1);
}

static r_obj* wng_call = NULL;
void r_warn(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  r_eval_with_x(wng_call, KEEP(r_chr(buf)), r_envs.ns);

  FREE(1);
}

static r_obj* err_call = NULL;
void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  r_eval_with_x(err_call, KEEP(r_chr(buf)), r_envs.ns);

  while (1); // No return
}

void r_cnd_signal(r_obj* cnd) {
  r_eval_with_x(cnd_signal_call, cnd, r_envs.base);
}


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

// For `R_interrupts_suspended`
#include <R_ext/GraphicsEngine.h>
#include <R_ext/GraphicsDevice.h>

#ifdef _WIN32
#include <Rembedded.h>
void r_interrupt() {
  UserBreak = 1;
  R_CheckUserInterrupt();
}
#else
#include <Rinterface.h>
void r_interrupt() {
  Rf_onintr();
}
#endif

enum r_cnd_type r_cnd_type(r_obj* cnd) {
  r_obj* classes = r_class(cnd);
  if (r_typeof(cnd) != R_TYPE_list ||
      r_typeof(classes) != R_TYPE_character) {
    goto error;
  }

  r_obj* const * v_classes = r_chr_cbegin(classes);
  r_ssize n_classes = r_length(classes);

  for (r_ssize i = n_classes - 2; i >= 0; --i) {
    r_obj* class_str = v_classes[i];

    if (class_str == r_strs.error) {
      return R_CND_TYPE_error;
    }
    if (class_str == r_strs.warning) {
      return R_CND_TYPE_warning;
    }
    if (class_str == r_strs.message) {
      return R_CND_TYPE_message;
    }
    if (class_str == r_strs.interrupt) {
      return R_CND_TYPE_interrupt;
    }
  }

  if (r_inherits(cnd, "condition")) {
    return R_CND_TYPE_condition;
  }

 error:
  r_abort("`cnd` is not a condition object.");
}


void r_init_library_cnd() {
  msg_call = r_parse("message(x)");
  r_preserve(msg_call);

  wng_call = r_parse("warning(x, call. = FALSE)");
  r_preserve(wng_call);

  err_call = r_parse("rlang::abort(x, call = NULL)");
  r_preserve(err_call);

  cnd_signal_call = r_parse("rlang::cnd_signal(x)");
  r_preserve(cnd_signal_call);

  r_stop_internal = (__attribute__((noreturn)) void (*)(const char*, const char*, ...)) R_GetCCallable("rlang", "rlang_stop_internal");
}

static
r_obj* cnd_signal_call = NULL;
