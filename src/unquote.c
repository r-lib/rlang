#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "utils.h"

SEXP interp_walk(SEXP x, SEXP env);
SEXP interp_arguments(SEXP x, SEXP env);


int bang_level(SEXP x) {
  if (!is_lang(x, "!"))
    return 0;

  SEXP arg = CDR(x);
  if (TYPEOF(arg) == NILSXP || !is_lang(CAR(arg), "!"))
    return 1;

  arg = CDR(CAR(arg));
  if (TYPEOF(arg) == NILSXP || !is_lang(CAR(arg), "!"))
    return 2;

  return 3;
}

int is_uq_sym(SEXP x) {
  if (TYPEOF(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQ") || is_sym(x, "UQE") || is_sym(x, "!!");
}
int is_uqf_sym(SEXP x) {
  if (TYPEOF(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQF");
}
int is_splice_sym(SEXP x) {
  if (TYPEOF(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQS") || is_sym(x, "!!!");
}

SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x);
  if (bang == 3 || is_any_call(x, is_splice_sym))
    Rf_errorcall(R_NilValue, "Cannot splice at top-level");
  else if (bang == 2) {
    x = CADR(x);
    SETCAR(x, Rf_install("UQ"));
  }
  return x;
}
SEXP replace_triple_bang(SEXP nxt, SEXP cur) {
  if (bang_level(CAR(nxt)) == 3) {
    nxt = CDAR(nxt);
    nxt = CDAR(nxt);
    SETCAR(CAR(nxt), Rf_install("UQS"));
    SETCDR(nxt, CDR(CDR(cur)));
  }
  return nxt;
}

// Change call name to prevent the formula from self-evaluating
SEXP guard_formula(SEXP f) {
  SEXP guard = PROTECT(LCONS(Rf_install("_F"), CDR(f)));
  Rf_copyMostAttrib(f, guard);
  UNPROTECT(1);
  return guard;
}

SEXP as_quosure(SEXP x) {
  static SEXP quo_classes = NULL;
  if (!quo_classes) {
    quo_classes = Rf_allocVector(STRSXP, 2);
    R_PreserveObject(quo_classes);
    SET_STRING_ELT(quo_classes, 0, Rf_mkChar("quosure"));
    SET_STRING_ELT(quo_classes, 1, Rf_mkChar("formula"));
  }

  static SEXP sym_environment = NULL;
  if (!sym_environment)
    sym_environment = Rf_install(".Environment");

  SEXP quo = PROTECT(Rf_shallow_duplicate(x));
  Rf_setAttrib(quo, R_ClassSymbol, quo_classes);
  Rf_setAttrib(quo, sym_environment, Rf_getAttrib(x, sym_environment));

  UNPROTECT(1);
  return quo;
}

void unquote_check(SEXP x) {
  if (CDR(x) == R_NilValue)
    Rf_errorcall(R_NilValue, "`UQ()` must be called with an argument");
}

SEXP unquote(SEXP x, SEXP env, SEXP uq_sym) {
  if (is_sym(uq_sym, "!!"))
    uq_sym = Rf_install("UQE");

  // Inline unquote function before evaluation because even `::` might
  // not be available in interpolation environment.
  SEXP uq_fun = rlang_fun(uq_sym);

  SEXP uq_call = PROTECT(Rf_lang2(uq_fun, x));
  SEXP res = Rf_eval(uq_call, env);

  if (is_formula(res)) {
    res = as_quosure(res);
  }

  UNPROTECT(1);
  return res;
}
SEXP unquote_prefixed_uq(SEXP x, SEXP env) {
  SEXP uq_sym = CADR(CDAR(x));
  SEXP unquoted = PROTECT(unquote(CADR(x), env, uq_sym));
  SETCDR(CDAR(x), CONS(unquoted, R_NilValue));
  UNPROTECT(1);
  if (is_rlang_prefixed(x, NULL))
    x = CADR(CDAR(x));
  else
    x = CAR(x);
  return x;
}
SEXP unquote_prefixed_uqf(SEXP x, SEXP env) {
  SEXP uqf_sym = CADR(CDAR(x));
  SEXP uqf_fun = rlang_fun(uqf_sym);
  SEXP uqf_call = PROTECT(Rf_lang2(uqf_fun, CADR(x)));
  SEXP unquoted = PROTECT(Rf_eval(uqf_call, env));
  SETCDR(CDAR(x), CONS(unquoted, R_NilValue));
  x = CADR(CDAR(x));
  x = guard_formula(x);
  UNPROTECT(2);
  return x;
}
SEXP splice_nxt(SEXP cur, SEXP nxt, SEXP env) {
  SETCAR(CAR(nxt), rlang_fun(Rf_install("UQS")));

  // UQS() does error checking and returns a pair list
  SEXP args_lsp = PROTECT(Rf_eval(CAR(nxt), env));

  if (args_lsp == R_NilValue) {
    SETCDR(cur, CDR(nxt));
  } else {
    // Insert args_lsp into existing pairlist of args
    SEXP last_arg = last_cons(args_lsp);
    SETCDR(last_arg, CDR(nxt));
    SETCDR(cur, args_lsp);
  }

  UNPROTECT(1);
  return cur;
}

SEXP interp_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x))
    return x;

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(x, &ipx);

  x = replace_double_bang(x);

  if (is_prefixed_call(x, is_uq_sym)) {
    unquote_check(x);
    REPROTECT(x = unquote_prefixed_uq(x, env), ipx);
  } else if (is_any_call(x, is_uq_sym)) {
    unquote_check(x);
    SEXP uq_sym = CAR(x);
    REPROTECT(x = unquote(CADR(x), env, uq_sym), ipx);
  } else if (is_rlang_prefixed(x, is_uqf_sym)) {
    REPROTECT(x = unquote_prefixed_uqf(x, env), ipx);
  } else if (is_formula(x) && !Rf_inherits(x, "quosure")) {
    // Guard but don't unquote: environment should be recorded in the
    // formula lazily
    REPROTECT(x = guard_formula(x), ipx);
    x = interp_arguments(x, env);
  } else if (is_lang(x, "UQF")) {
    // Guard and unquote: environment is recorded here
    REPROTECT(x = Rf_eval(x, env), ipx);
    REPROTECT(x = guard_formula(x), ipx);
  } else {
    x = interp_arguments(x, env);
  }

  UNPROTECT(1);
  return x;
}

SEXP interp_arguments(SEXP x, SEXP env) {
  for(SEXP cur = x; cur != R_NilValue; cur = CDR(cur)) {
    SETCAR(cur, interp_walk(CAR(cur), env));

    SEXP nxt = CDR(cur);
    nxt = replace_triple_bang(nxt, cur);
    if (is_rlang_call(CAR(nxt), is_splice_sym)) {
      cur = splice_nxt(cur, nxt, env);
      cur = nxt; // Don't interpolate unquoted stuff
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env) {
  if (!Rf_isLanguage(x))
    return x;
  if (!Rf_isEnvironment(env))
    Rf_errorcall(R_NilValue, "`env` must be an environment");

  x = PROTECT(Rf_duplicate(x));
  x = interp_walk(x, env);

  UNPROTECT(1);
  return x;
}
