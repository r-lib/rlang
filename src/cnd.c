#include "rlang.h"

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

  SEXP lang = KEEP(Rf_lcons(r_sym("message"), string(buf)));
  Rf_eval(lang, R_BaseEnv);
  FREE(1);
}
void r_warn(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  SEXP args = KEEP(r_new_pairlist(Rf_ScalarLogical(0)));
  SET_TAG(args, r_sym("call."));

  args = KEEP(r_new_node(string(buf), args));
  SEXP lang = KEEP(Rf_lcons(r_sym("warning"), args));

  Rf_eval(lang, R_BaseEnv);
  FREE(3);
}
void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  Rf_errorcall(R_NilValue, buf);
}
SEXP interp_str(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  return string(buf);
}

static
SEXP new_condition_names(SEXP data) {
  if (!is_named(data))
    r_abort("Conditions must have named data fields");

  SEXP data_nms = sxp_names(data);

  if (chr_has(data_nms, "message"))
    r_abort("Conditions can't have a `message` data field");

  SEXP nms = KEEP(Rf_allocVector(STRSXP, r_length(data) + 1));
  mut_chr_at(nms, 0, r_string("message"));
  vec_copy_n(data_nms, r_length(data), nms, 1, 0);

  FREE(1);
  return nms;
}
SEXP new_condition(SEXP type, SEXP data, SEXP msg) {
  if (!is_null(msg) && !is_string(msg))
    r_abort("Condition message must be a string");

  int n_data = r_length(data);
  SEXP cnd = KEEP(Rf_allocVector(VECSXP, n_data + 1));

  mut_list_at(cnd, 0, msg);
  vec_copy_n(data, n_data, cnd, 1, 0);

  mut_names(cnd, new_condition_names(data));
  mut_class(cnd, chr_append(type, r_string("condition")));

  FREE(1);
  return cnd;
}

bool is_condition(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "condition");
}

static
SEXP with_muffle_lang(SEXP signal) {
  static SEXP muffle_arg = NULL;
  if (!muffle_arg) {
    muffle_arg = r_new_pairlist(rlang_obj("muffle"));
    R_PreserveObject(muffle_arg);
    SET_TAG(muffle_arg, r_sym("muffle"));
  }

  SEXP args = KEEP(r_new_node(signal, muffle_arg));
  SEXP lang = Rf_lcons(r_sym("withRestarts"), args);

  FREE(1);
  return lang;
}
static
void cnd_signal_impl(const char* signaller, SEXP cnd, bool mufflable) {
  int n_protect = 0;

  if (TYPEOF(cnd) == STRSXP) {
    cnd = KEEP(new_condition(cnd, R_NilValue, R_NilValue));
    ++n_protect;
  } else if (!is_condition(cnd)) {
    r_abort("`cnd` must be a condition");
  }

  SEXP lang = KEEP(Rf_lang2(r_sym(signaller), cnd));
  ++n_protect;

  if (mufflable) {
    SEXP classes = KEEP(chr_prepend(sxp_class(cnd), r_string("mufflable")));
    ++n_protect;
    SETCADR(lang, set_class(cnd, classes));

    lang = KEEP(with_muffle_lang(lang));
    ++n_protect;
  }

  Rf_eval(lang, R_BaseEnv);
  FREE(n_protect);
}

void cnd_signal(SEXP cnd, bool mufflable) {
  cnd_signal_impl("signalCondition", cnd, mufflable);
}
void cnd_signal_error(SEXP cnd, bool mufflable) {
  cnd_signal_impl("stop", cnd, mufflable);
}

SEXP rlang_cnd_signal(SEXP cnd, SEXP mufflable) {
  cnd_signal(cnd, r_as_bool(mufflable));
  return R_NilValue;
}
SEXP rlang_cnd_signal_error(SEXP cnd, SEXP mufflable) {
  cnd_signal_error(cnd, r_as_bool(mufflable));
  return R_NilValue;
}
