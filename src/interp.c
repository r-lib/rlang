#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "utils.h"

int bang_level(SEXP x) {
  if (!is_call_to(x, "!"))
    return 0;

  SEXP arg = CDR(x);
  if (TYPEOF(arg) == NILSXP || !is_call_to(CAR(arg), "!"))
    return 1;

  arg = CDR(CAR(arg));
  if (TYPEOF(arg) == NILSXP || !is_call_to(CAR(arg), "!"))
    return 2;

  return 3;
}

int is_unquote(SEXP x) {
  return is_call_to(x, "UQ") || is_call_to(x, "!!");
}
int is_splice(SEXP x) {
  return is_call_to(x, "UQS") || is_call_to(x, "!!!");
}

int is_prefixed_call(SEXP x) {
  return
    is_call_to(x, "$") ||
    is_call_to(x, "@") ||
    is_call_to(x, "::") ||
    is_call_to(x, ":::");
}
int is_prefixed_unquote(SEXP x) {
  if (!is_prefixed_call(CAR(x)))
    return 0;

  SEXP args = CDAR(x);
  return CADR(args) == Rf_install("!!");
}

SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x);
  if (bang == 3 || is_splice(x))
    Rf_error("Cannot splice at top-level");
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

SEXP unquote(SEXP x, SEXP env, SEXP data) {
  SEXP uq_call = PROTECT(Rf_lang3(Rf_install("UQ"), x, data));
  SEXP res = PROTECT(Rf_eval(uq_call, env));
  UNPROTECT(2);
  return res;
}

SEXP splice(SEXP cur, SEXP nxt, SEXP env) {
  // UQS() does error checking and returns a pair list
  SEXP args_lsp = Rf_eval(CAR(nxt), env);

  // Insert args_lsp into existing pairlist of args
  SEXP last_arg = findLast(args_lsp);
  SETCDR(last_arg, CDR(nxt));
  SETCDR(cur, args_lsp);
  return cur;
}


SEXP interp_arguments(SEXP x, SEXP env, SEXP data);

SEXP interp_walk(SEXP x, SEXP env, SEXP data)  {
  if (!Rf_isLanguage(x))
    return x;

  x = replace_double_bang(x);

  if (is_unquote(x)) {
    x = unquote(CADR(x), env, data);
  } else if (is_call_to(x, "UQF")) {
    x = Rf_eval(x, env);
  } else if (is_prefixed_unquote(x)) {
    SEXP unquoted = PROTECT(unquote(CADR(x), env, data));
    SETCDR(CDAR(x), CONS(unquoted, R_NilValue));
    UNPROTECT(1);
    x = CAR(x);
  } else {
    x = interp_arguments(x, env, data);
  }

  return x;
}

SEXP interp_arguments(SEXP x, SEXP env, SEXP data) {
  for(SEXP cur = x; cur != R_NilValue; cur = CDR(cur)) {
    SETCAR(cur, interp_walk(CAR(cur), env, data));
    SEXP nxt = CDR(cur);

    nxt = replace_triple_bang(nxt, cur);
    if (is_splice(CAR(nxt)))
      cur = splice(cur, nxt, env);
  }

  return x;
}

SEXP interp_(SEXP x, SEXP env, SEXP data) {
  if (!Rf_isLanguage(x))
    return x;

  if (!Rf_isEnvironment(env))
    Rf_error("`env` must be an environment");

  return interp_walk(Rf_duplicate(x), env, data);
}

