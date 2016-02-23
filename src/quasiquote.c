#define R_NO_REMAP
#include <R.h>
#include <Rdefines.h>
#include <stdbool.h>

bool is_scalar(SEXP x) {
  return Rf_isVectorAtomic(x) && Rf_length(x) == 1;
}

// Is a call the correct form to be unquoted? ------------------------------------

bool is_parens_call(SEXP x) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP parens = Rf_install("(");
  SEXP fun = CAR(x);

  return Rf_isSymbol(fun) && fun == parens;
}

bool is_brace_call(SEXP x) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP fun = CAR(x);
  return Rf_isSymbol(fun) && fun == R_BraceSymbol;
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

bool is_unquote_splice(SEXP x) {
  if (!is_parens_call(x))
    return false;

  if (!has_one_argument(x))
    return false;

  // CADR(x) = first argument in call
  if (!is_brace_call(CADR(x)))
    return false;

  return true;
}


// Helpers for testing
SEXP is_unquote_c(SEXP x) {
  return Rf_ScalarLogical(is_unquote(x));
}
SEXP is_unquote_splice_c(SEXP x) {
  return Rf_ScalarLogical(is_unquote_splice(x));
}

// Quasiquotation --------------------------------------------------------------

SEXP findLast(SEXP x) {
  if (!Rf_isPairList(x))
    Rf_error("x must be a pairlist");

  SEXP cons = x;
  while(CDR(cons) != R_NilValue)
    cons = CDR(cons);

  return cons;
}

SEXP quasiquote_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x))
    return x;

  if (is_unquote(x))
    return Rf_eval(x, env);

  // Recursive case
  for(SEXP cur = x; cur != R_NilValue; cur = CDR(cur)) {
    SETCAR(cur, quasiquote_walk(CAR(cur), env));

    SEXP nxt = CDR(cur);
    if (is_unquote_splice(CAR(nxt))) {
      SEXP args_list = Rf_eval(CAR(nxt), env);
      if (!Rf_isNewList(args_list)) {
        Rf_error("({}) must evaluate to a list, not a %s", Rf_type2char(TYPEOF(args_list)));
      }
      SEXP args_pl = Rf_VectorToPairList(args_list);
      SEXP last_arg = findLast(args_pl);

      SETCDR(last_arg, CDR(nxt));
      SETCDR(cur, args_pl);
    }
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

  return quasiquote_walk(Rf_duplicate(x), env);
}

