#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include "utils.h"

int is_bang(SEXP x) {
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

SEXP interp_walk(SEXP x, SEXP env, SEXP data)  {
  if (!Rf_isLanguage(x))
    return x;

  int bang = is_bang(x);
  if (bang == 3 || is_call_to(x, "uqs"))
    Rf_error("Cannot splice at top-level");

  if (bang == 2) {
    x = CADR(x);
    SETCAR(x, Rf_install("uq"));
  }

  if (is_call_to(x, "uq")) {
    SEXP uq_call = PROTECT(Rf_lang3(Rf_install("uq"), CADR(x), data));
    SEXP res = PROTECT(Rf_eval(uq_call, env));
    UNPROTECT(2);
    return res;
  }

  if (is_call_to(x, "uqf")) {
    return Rf_eval(x, env);
  }

  // Recursive case
  for(SEXP cur = x; cur != R_NilValue; cur = CDR(cur)) {
    SETCAR(cur, interp_walk(CAR(cur), env, data));
    SEXP nxt = CDR(cur);

    if (is_bang(CAR(nxt)) == 3) {
      nxt = CDAR(nxt);
      nxt = CDAR(nxt);
      SETCAR(CAR(nxt), Rf_install("uqs"));
      SETCDR(nxt, CDR(CDR(cur)));
    }

    if (is_call_to(CAR(nxt), "uqs")) {
      // uqs() does error checking and returns a pair list
      SEXP args_lsp = Rf_eval(CAR(nxt), env);

      // Insert args_lsp into existing pairlist of args
      SEXP last_arg = findLast(args_lsp);
      SETCDR(last_arg, CDR(nxt));
      SETCDR(cur, args_lsp);
    }
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

