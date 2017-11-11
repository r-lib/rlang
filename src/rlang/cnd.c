#include "rlang.h"

SEXP rlang_ns_get(const char* name);


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

  SEXP lang = KEEP(r_build_call_node(r_sym("message"), r_scalar_chr(buf)));
  r_eval(lang, R_BaseEnv);
  FREE(1);
}
void r_warn(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  SEXP args = KEEP(r_build_node(Rf_ScalarLogical(0), r_null));
  r_node_poke_tag(args, r_sym("call."));

  args = KEEP(r_build_node(r_scalar_chr(buf), args));
  SEXP lang = KEEP(r_build_call_node(r_sym("warning"), args));

  r_eval(lang, R_BaseEnv);
  FREE(3);
}
void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  Rf_errorcall(r_null, buf);

  while (1); // No return
}
SEXP r_interp_str(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  return r_scalar_chr(buf);
}

static SEXP new_condition_names(SEXP data) {
  if (!r_is_named(data)) {
    r_abort("Conditions must have named data fields");
  }

  SEXP data_nms = r_names(data);

  if (r_chr_has(data_nms, "message")) {
    r_abort("Conditions can't have a `message` data field");
  }

  SEXP nms = KEEP(r_new_vector(STRSXP, r_length(data) + 1));
  r_chr_poke(nms, 0, r_string("message"));
  r_vec_poke_n(nms, 1, data_nms, 0, r_length(nms) - 1);

  FREE(1);
  return nms;
}
SEXP r_new_condition(SEXP type, SEXP data, SEXP msg) {
  if (!r_is_null(msg) && !r_is_scalar_character(msg)) {
    r_abort("Condition message must be a string");
  }

  r_size_t n_data = r_length(data);
  SEXP cnd = KEEP(r_new_vector(VECSXP, n_data + 1));

  r_list_poke(cnd, 0, msg);
  r_vec_poke_n(cnd, 1, data, 0, r_length(cnd) - 1);

  r_poke_names(cnd, KEEP(new_condition_names(data)));
  r_poke_class(cnd, KEEP(chr_append(type, r_string("condition"))));

  FREE(3);
  return cnd;
}

static SEXP with_muffle_lang(SEXP signal) {
  static SEXP muffle_node = NULL;
  if (!muffle_node) {
    muffle_node = r_build_pairlist(rlang_ns_get("muffle"));
    R_PreserveObject(muffle_node);
    r_node_poke_tag(muffle_node, r_sym("muffle"));
  }

  SEXP args = r_build_node(signal, muffle_node);
  SEXP lang = r_build_call_node(r_sym("withRestarts"), args);

  return lang;
}
static void cnd_signal_impl(const char* signaller, SEXP cnd, bool mufflable) {
  int n_protect = 0;

  if (r_kind(cnd) == STRSXP) {
    cnd = KEEP(r_new_condition(cnd, r_null, r_null));
    ++n_protect;
  } else if (!r_is_condition(cnd)) {
    r_abort("`cnd` must be a condition");
  }

  SEXP lang = KEEP(r_build_call1(r_sym(signaller), cnd));
  ++n_protect;

  if (mufflable) {
    SEXP classes = KEEP(chr_prepend(r_get_class(cnd), r_string("mufflable")));
    ++n_protect;
    SETCADR(lang, r_set_class(cnd, classes));

    lang = KEEP(with_muffle_lang(lang));
    ++n_protect;
  }

  r_eval(lang, R_BaseEnv);
  FREE(n_protect);
}

void r_cnd_signal(SEXP cnd, bool mufflable) {
  cnd_signal_impl("signalCondition", cnd, mufflable);
}
void r_cnd_inform(SEXP cnd, bool mufflable) {
  cnd_signal_impl("message", cnd, mufflable);
}
void r_cnd_warn(SEXP cnd, bool mufflable) {
  cnd_signal_impl("warning", cnd, mufflable);
}
void r_cnd_abort(SEXP cnd, bool mufflable) {
  cnd_signal_impl("stop", cnd, mufflable);
}
