#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdbool.h>

static
SEXP find_dots(SEXP env) {
  if (TYPEOF(env) != ENVSXP) {
    Rf_errorcall(R_NilValue, "`env` is a not an environment");
  }

  SEXP dots = PROTECT(Rf_findVarInFrame3(env, R_DotsSymbol, TRUE));
  if (dots == R_UnboundValue) {
    Rf_errorcall(R_NilValue, "No ... found");
  }

  UNPROTECT(1);
  return dots;
}

SEXP ffi_ellipsis_dots(SEXP env, SEXP auto_name_) {
  int auto_name = Rf_asLogical(auto_name_);

  SEXP dots = PROTECT(find_dots(env));

  // Empty dots
  if (dots == R_MissingArg) {
    UNPROTECT(1);
    return Rf_allocVector(VECSXP, 0);
  }

  R_len_t n = Rf_length(dots);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, n));
  Rf_setAttrib(out, R_NamesSymbol, names);

  for (R_len_t i = 0; i < n; ++i) {
    SET_VECTOR_ELT(out, i, CAR(dots));

    SEXP name = TAG(dots);
    if (TYPEOF(name) == SYMSXP) {
      SET_STRING_ELT(names, i, PRINTNAME(name));
    } else {
      if (auto_name) {
        char buffer[20];
        snprintf(buffer, 20, "..%i", i + 1);
        SET_STRING_ELT(names, i, Rf_mkChar(buffer));
      } else {
        SET_STRING_ELT(names, i, NA_STRING);
      }
    }

    dots = CDR(dots);
  }

  UNPROTECT(3);
  return out;
}

static
bool promise_forced(SEXP x) {
  if (TYPEOF(x) != PROMSXP) {
    return true;
  } else {
    return PRVALUE(x) != R_UnboundValue;
  }
}
SEXP ffi_ellipsis_promise_forced(SEXP x) {
  return Rf_ScalarLogical(promise_forced(x));
}

SEXP ffi_ellipsis_dots_used(SEXP env) {
  SEXP dots = PROTECT(find_dots(env));

  if (dots == R_MissingArg) {
    UNPROTECT(1);
    return Rf_ScalarLogical(true);
  }

  while (dots != R_NilValue) {
    SEXP elt = CAR(dots);

    if (!promise_forced(elt)) {
      UNPROTECT(1);
      return Rf_ScalarLogical(false);
    }

    dots = CDR(dots);
  }

  UNPROTECT(1);
  return Rf_ScalarLogical(true);
}

SEXP ellipsis_eval_bare(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}
