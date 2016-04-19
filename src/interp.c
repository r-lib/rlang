#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include "utils.h"

SEXP interp_walk(SEXP x, SEXP env, SEXP data)  {
  if (!Rf_isLanguage(x))
    return x;

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
    if (is_call_to(CAR(nxt), "uqs")) {
      // uqs() does error checking and returns a pair list
      SEXP args_pl = Rf_eval(CAR(nxt), env);

      // Insert args_pl into existing pairlist of args
      SEXP last_arg = findLast(args_pl);
      SETCDR(last_arg, CDR(nxt));
      SETCDR(cur, args_pl);
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

