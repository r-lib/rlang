#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include <stdbool.h>

bool is_scalar(SEXP x) {
  return Rf_isVectorAtomic(x) && Rf_length(x) == 1;
}

// Is a call the write form to be unquoted? ------------------------------------

bool is_parens_call(SEXP x) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP parens = Rf_install("(");
  SEXP fun = CAR(x);

  return Rf_isSymbol(fun) && fun == parens;
}

bool has_one_argument(SEXP x) {
  SEXP rest = CDR(x);
  // Must have a first argument
  if (rest == R_NilValue)
    return false;
  // Must not have a second argument
  if (CDR(rest) != R_NilValue)
    return false;

  return true;
}

bool is_unquote(SEXP x) {
  if (!is_parens_call(x))
    return false;

  if (!has_one_argument(x))
    return false;

  // CADR(x) = first argument in call
  if (!is_parens_call(CADR(x)))
    return false;

  return true;
}

SEXP is_unquote_c(SEXP x) {
  return Rf_ScalarLogical(is_unquote(x));
}

// Quasiquotation --------------------------------------------------------------

SEXP quasiquote_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x))
    return x;

  if (is_parens_call(x))
    return Rf_eval(x, env);

  // Recursive case
  for(SEXP cons = x; cons != R_NilValue; cons = CDR(cons)) {
    SETCAR(cons, quasiquote_walk(CAR(cons), env));
  }
  return x;
}

SEXP quasiquote_c(SEXP x, SEXP env) {
  if (!Rf_isLanguage(x) && !Rf_isSymbol(x) && !is_scalar(x)) {
    Rf_error("`x` must be a call, symbol, or scalar");
  }
  if (!Rf_isEnvironment(env)) {
    Rf_error("`env` must be an environment");
  }

  return quasiquote_walk(x, env);
}

