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

  SEXP parens = Rf_install(f);
  SEXP fun = CAR(x);

  return Rf_isSymbol(fun) && fun == parens;
}

bool has_one_argument(SEXP x) {
  if (!Rf_isLanguage(x))
    return false;

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
  if (!has_one_argument(x))
    return false;

  if (is_call_to(x, "(")) {
    return is_call_to(CADR(x), "(");
  } else if (is_call_to(x, "unquote")) {
    return true;
  } else {
    return false;
  }

}

bool is_unquote_splice(SEXP x) {
  if (!has_one_argument(x))
    return false;

  if (is_call_to(x, "(")) {
    return is_call_to(CADR(x), "{");
  } else if (is_call_to(x, "unquote_splice")) {
    return true;
  } else {
    return false;
  }

}

// Helpers for testing
SEXP is_unquote_c(SEXP x) {
  return Rf_ScalarLogical(is_unquote(x));
}
SEXP is_unquote_splice_c(SEXP x) {
  return Rf_ScalarLogical(is_unquote_splice(x));
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

  if (is_unquote(x))
    return Rf_eval(CADR(x), env);

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

