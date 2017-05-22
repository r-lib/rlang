#include "rlang.h"

#define BUFSIZE 8192


void r_abort(const char* fmt, ...) {
  char buf[BUFSIZE];
  va_list dots;
  va_start(dots, fmt);
  vsnprintf(buf, BUFSIZE, fmt, dots);
  va_end(dots);

  Rf_errorcall(R_NilValue, buf);
}
SEXP interp_str(const char* fmt, ...) {
  char buf[BUFSIZE];
  va_list dots;
  va_start(dots, fmt);
  vsnprintf(buf, BUFSIZE, fmt, dots);
  va_end(dots);

  // Make sure buffer is NULL terminated
  buf[BUFSIZE - 1] = '\0';

  return Rf_mkString(buf);
}

static
SEXP new_condition_names(SEXP data) {
  if (!is_named(data))
    r_abort("Conditions must have named data fields");

  SEXP data_nms = sxp_names(data);

  if (chr_has(data_nms, "message"))
    r_abort("Conditions can't have a `message` data field");

  SEXP nms = PROTECT(Rf_allocVector(STRSXP, Rf_length(data) + 1));
  mut_chr_at(nms, 0, as_r_string("message"));
  vec_copy_n(data_nms, Rf_length(data), nms, 1, 0);

  UNPROTECT(1);
  return nms;
}
SEXP new_condition(SEXP type, SEXP data, SEXP msg) {
  if (!is_null(msg) && !is_string(msg))
    r_abort("Condition message must be a string");

  int n_data = Rf_length(data);
  SEXP cnd = PROTECT(Rf_allocVector(VECSXP, n_data + 1));

  mut_list_at(cnd, 0, msg);
  vec_copy_n(data, n_data, cnd, 1, 0);

  mut_names(cnd, new_condition_names(data));
  mut_class(cnd, chr_append(type, as_r_string("condition")));

  UNPROTECT(1);
  return cnd;
}

bool is_condition(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "condition");
}

static
SEXP with_muffle_lang(SEXP signal) {
  static SEXP muffle_arg = NULL;
  if (!muffle_arg) {
    muffle_arg = Rf_cons(rlang_fun(sym("muffle")), R_NilValue);
    R_PreserveObject(muffle_arg);
    SET_TAG(muffle_arg, sym("muffle"));
  }

  SEXP args = PROTECT(Rf_cons(signal, muffle_arg));
  SEXP lang = PROTECT(Rf_lcons(sym("withRestarts"), args));

  UNPROTECT(2);
  return lang;
}
static
void cnd_signal_impl(const char* signaller, SEXP cnd, bool mufflable) {
  int n_protect = 0;

  if (TYPEOF(cnd) == STRSXP) {
    cnd = PROTECT(new_condition(cnd, R_NilValue, R_NilValue));
    ++n_protect;
  } else if (!is_condition(cnd)) {
    r_abort("`cnd` must be a condition");
  }

  SEXP lang = PROTECT(Rf_lang2(sym(signaller), cnd));
  ++n_protect;

  if (mufflable) {
    SEXP classes = PROTECT(chr_prepend(sxp_class(cnd), as_r_string("mufflable")));
    ++n_protect;
    SETCADR(lang, set_class(cnd, classes));

    lang = PROTECT(with_muffle_lang(lang));
    ++n_protect;
  }

  Rf_eval(lang, R_BaseEnv);
  UNPROTECT(n_protect);
}

void cnd_signal(SEXP cnd, bool mufflable) {
  cnd_signal_impl("signalCondition", cnd, mufflable);
}
void cnd_signal_error(SEXP cnd, bool mufflable) {
  cnd_signal_impl("stop", cnd, mufflable);
}

SEXP rlang_cnd_signal(SEXP cnd, SEXP mufflable) {
  cnd_signal(cnd, as_bool(mufflable));
  return cnd;
}
SEXP rlang_cnd_signal_error(SEXP cnd, SEXP mufflable) {
  cnd_signal_error(cnd, as_bool(mufflable));
  return cnd;
}
