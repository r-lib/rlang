#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include <stdbool.h>

bool is_scalar(SEXP x) {
  return Rf_isVectorAtomic(x) && Rf_length(x) == 1;
}

// Is a call the correct form to be unquoted? ------------------------------------

bool is_call_to(SEXP x, const char* f) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP fun = CAR(x);
  if (!Rf_isSymbol(fun))
    return false;

  return fun == Rf_install(f);
}

SEXP rhs(SEXP f) {
  if (!Rf_inherits(f, "formula"))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  switch (Rf_length(f)) {
  case 2: return CADR(f);
  case 3: return CADDR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}


// Quasiquotation --------------------------------------------------------------

SEXP findLast(SEXP x) {
  SEXP cons = x;
  while(CDR(cons) != R_NilValue)
    cons = CDR(cons);

  return cons;
}

SEXP quasiquote_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x))
    return x;

  if (is_call_to(x, "uq"))
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

