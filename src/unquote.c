#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "utils.h"

SEXP interp_walk(SEXP x, SEXP env);
SEXP interp_arguments(SEXP x, SEXP env);


int bang_level(SEXP x) {
  if (!is_call(x, "!"))
    return 0;

  SEXP arg = CDR(x);
  if (TYPEOF(arg) == NILSXP || !is_call(CAR(arg), "!"))
    return 1;

  arg = CDR(CAR(arg));
  if (TYPEOF(arg) == NILSXP || !is_call(CAR(arg), "!"))
    return 2;

  return 3;
}

int is_unquote(SEXP x) {
  return
    is_call(x, "UQ") ||
    is_call(x, "UQE") ||
    is_call(x, "!!");
}
int is_splice(SEXP x) {
  return is_call(x, "UQS") || is_call(x, "!!!");
}

int is_prefixed_call(SEXP x, const char* fn) {
  SEXP head = CAR(x);
  if (!(is_call(head, "$") ||
        is_call(head, "@") ||
        is_call(head, "::") ||
        is_call(head, ":::")))
    return 0;

  if (fn == NULL)
    return 1;

  SEXP args = CDAR(x);
  SEXP sym = CADR(args);
  return sym == Rf_install(fn);
}
int is_prefixed_uq(SEXP x) {
  if (!is_prefixed_call(x, NULL))
    return 0;

  SEXP args = CDAR(x);
  SEXP sym = CADR(args);
  const char* nm =  CHAR(PRINTNAME(sym));

  if (strcmp(nm, "!!") == 0 || strcmp(nm, "UQE") == 0) {
    return 1;
  } else {
    return strcmp(nm, "UQ") == 0;
  }
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

SEXP unquote_sym(SEXP sym) {
  const char* nm = CHAR(PRINTNAME(sym));
  if (strcmp(nm, "!!") == 0)
    return Rf_install("UQE");
  else
    return sym;
}
SEXP unquote(SEXP x, SEXP env, SEXP uq_sym) {
  uq_sym = unquote_sym(uq_sym);
  SEXP uq_call = PROTECT(Rf_lang2(uq_sym, x));

  SEXP res = Rf_eval(uq_call, env);
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);

  UNPROTECT(2);
  return res;
}

SEXP splice_nxt(SEXP cur, SEXP nxt, SEXP env) {
  // UQS() does error checking and returns a pair list
  SEXP args_lsp = PROTECT(Rf_eval(CAR(nxt), env));

  // Insert args_lsp into existing pairlist of args
  SEXP last_arg = last_cons(args_lsp);
  SETCDR(last_arg, CDR(nxt));
  SETCDR(cur, args_lsp);

  UNPROTECT(1);
  return cur;
}

// Change call name to prevent the formula from self-evaluating
SEXP guard_formula(SEXP f) {
  SEXP guard = PROTECT(LCONS(Rf_install("_F"), CDR(f)));
  Rf_copyMostAttrib(f, guard);
  UNPROTECT(1);
  return guard;
}

SEXP interp_walk(SEXP x, SEXP env)  {
  if (!Rf_isLanguage(x) || (is_formula(x)))
    return x;

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(x, &ipx);

  x = replace_double_bang(x);

  // Deal with unquoting
  if (is_prefixed_uq(x)) {
    SEXP uq_sym = CADR(CDAR(x));
    SEXP unquoted = PROTECT(unquote(CADR(x), env, uq_sym));
    SETCDR(CDAR(x), CONS(unquoted, R_NilValue));
    UNPROTECT(1);
    x = CAR(x);
  } else if (is_unquote(x)) {
    SEXP uq_sym = CAR(x);
    REPROTECT(x = unquote(CADR(x), env, uq_sym), ipx);
  } else if (is_call(x, "UQF")) {
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
    if (is_splice(CAR(nxt))) {
      cur = splice_nxt(cur, nxt, env);
      cur = nxt; // Don't interpolate unquoted stuff
    }
  }

  return x;
}

SEXP interp_(SEXP x, SEXP env) {
  if (!Rf_isLanguage(x))
    return x;
  if (!Rf_isEnvironment(env))
    Rf_error("`env` must be an environment");

  x = PROTECT(Rf_duplicate(x));
  x = interp_walk(x, env);

  UNPROTECT(1);
  return x;
}
