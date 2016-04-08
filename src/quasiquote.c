#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include "utils.h"

SEXP quasiquote_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x))
    return x;

  if (is_call_to(x, "uq") || is_call_to(x, "uqf"))
    return Rf_eval(x, env);

  // Recursive case
  for(SEXP cur = x; cur != R_NilValue; cur = CDR(cur)) {
    SETCAR(cur, quasiquote_walk(CAR(cur), env));

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

SEXP quasiquote_c(SEXP x, SEXP env) {
  if (!Rf_isLanguage(x))
    return x;

  if (!Rf_isEnvironment(env))
    Rf_error("`env` must be an environment");

  return quasiquote_walk(Rf_duplicate(x), env);
}

