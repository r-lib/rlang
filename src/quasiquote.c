#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include <stdbool.h>

// Goal: write quasiquote function
// 1. Takes call, name, or vector as input.
//    Names and vectors are returned as is.
// 2. Walk through calls::
//    non-calls returned as is.
//    calls:
//    1. Check if call to `(` then `(`: if so, eval and replace current node
//    2. Otherwise recursive
// 3. Next implement ({ for unquote splice - needs to evaluate, check that
//     it returns a list and then add to call.

bool is_scalar(SEXP x) {
  return Rf_isVectorAtomic(x) && Rf_length(x) == 1;
}

bool is_parens_call(SEXP x) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP parens = Rf_install("(");
  SEXP fun = CAR(x);

  return Rf_isSymbol(fun) && fun == parens;
}

bool is_unquote(SEXP x) {
  // Language object is a pairlist: first element should be a call to parens
  if (!is_parens_call(x))
    return false;

  SEXP rest = CDR(x);
  // Must have a first argument
  if (rest == R_NilValue)
    return false;
  // Must not have a second argument
  if (CDR(rest) != R_NilValue)
    return false;

  // First argument should also be a call to parens
  if (!is_parens_call(CAR(rest)))
    return false;

  return true;
}

SEXP is_unquote_(SEXP x) {
  return Rf_ScalarLogical(is_unquote(x));
}

SEXP quasiquote_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x))
    return x;


}


SEXP quasiquote(SEXP x, SEXP env) {
  if (!Rf_isLanguage(x) && !Rf_isSymbol(x) && !is_scalar(x)) {
    Rf_error("`x` must be a call, symbol, or scalar");
  }
  if (!Rf_isEnvironment(env)) {
    Rf_error("`env` must be an environment");
  }

  return quasiquote_walk(x, env);
}

