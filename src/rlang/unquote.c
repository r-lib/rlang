#include "rlang.h"

SEXP interp_lang(SEXP x, SEXP env, bool quosured);
SEXP interp_pairlist(SEXP x, SEXP env, bool quosured);


int bang_level(SEXP x) {
  if (!is_lang(x, "!"))
    return 0;

  SEXP arg = CDR(x);
  if (TYPEOF(arg) == NILSXP || !is_lang(CAR(arg), "!"))
    return 1;

  arg = CDR(CAR(arg));
  if (TYPEOF(arg) == NILSXP || !is_lang(CAR(arg), "!"))
    return 2;

  return 3;
}

int is_uq_sym(SEXP x) {
  if (TYPEOF(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQ") || is_sym(x, "UQE") || is_sym(x, "!!");
}
int is_splice_sym(SEXP x) {
  if (TYPEOF(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQS") || is_sym(x, "!!!");
}

SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x);
  if (bang == 3 || is_any_call(x, is_splice_sym))
    Rf_errorcall(R_NilValue, "Can't splice at top-level");
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

void unquote_check(SEXP x) {
  if (CDR(x) == R_NilValue)
    Rf_errorcall(R_NilValue, "`UQ()` must be called with an argument");
}

bool is_bare_formula(SEXP x) {
  return TYPEOF(x) == LANGSXP
      && CAR(x) == Rf_install("~")
      && !Rf_inherits(x, "quosure");
}
SEXP unquote(SEXP x, SEXP env, SEXP uq_sym, bool quosured) {
  if (is_sym(uq_sym, "!!"))
    uq_sym = Rf_install("UQE");

  // Inline unquote function before evaluation because even `::` might
  // not be available in interpolation environment.
  SEXP uq_fun = r_env_get(r_ns_env("rlang"), uq_sym);

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(uq_fun, &ipx);
  REPROTECT(uq_fun = Rf_lang2(uq_fun, x), ipx);

  SEXP unquoted;
  REPROTECT(unquoted = Rf_eval(uq_fun, env), ipx);

  if (!quosured && is_symbolic(unquoted))
    unquoted = Rf_lang2(Rf_install("quote"), unquoted);

  FREE(1);
  return unquoted;
}
SEXP unquote_prefixed_uq(SEXP x, SEXP env, bool quosured) {
  SEXP uq_sym = CADR(CDAR(x));
  SEXP unquoted = KEEP(unquote(CADR(x), env, uq_sym, quosured));
  SETCDR(CDAR(x), r_new_node_(unquoted, R_NilValue));
  FREE(1);

  if (is_rlang_prefixed(x, NULL))
    x = CADR(CDAR(x));
  else
    x = CAR(x);
  return x;
}
SEXP splice_nxt(SEXP cur, SEXP nxt, SEXP env) {
  static SEXP uqs_fun;
  if (!uqs_fun)
    uqs_fun = rlang_obj("UQS");
  SETCAR(CAR(nxt), uqs_fun);

  // UQS() does error checking and returns a pair list
  SEXP args_lsp = KEEP(Rf_eval(CAR(nxt), env));

  if (args_lsp == R_NilValue) {
    SETCDR(cur, CDR(nxt));
  } else {
    // Insert args_lsp into existing pairlist of args
    SEXP last_arg = last_cons(args_lsp);
    SETCDR(last_arg, CDR(nxt));
    SETCDR(cur, args_lsp);
  }

  FREE(1);
  return cur;
}
SEXP splice_value_nxt(SEXP cur, SEXP nxt, SEXP env) {
  SETCAR(CAR(nxt), rlang_obj("splice"));
  SETCAR(nxt, Rf_eval(CAR(nxt), env));
  return cur;
}

SEXP interp_lang(SEXP x, SEXP env, bool quosured)  {
  if (!Rf_isLanguage(x))
    return x;

  KEEP(x);
  x = replace_double_bang(x);

  if (is_prefixed_call(x, is_uq_sym)) {
    unquote_check(x);
    x = unquote_prefixed_uq(x, env, quosured);
  } else if (is_any_call(x, is_uq_sym)) {
    unquote_check(x);
    SEXP uq_sym = CAR(x);
    x = unquote(CADR(x), env, uq_sym, quosured);
  } else {
    x = interp_pairlist(x, env, quosured);
  }

  FREE(1);
  return x;
}

SEXP interp_pairlist(SEXP x, SEXP env, bool quosured) {
  for(SEXP cur = x; cur != R_NilValue; cur = CDR(cur)) {
    SETCAR(cur, interp_lang(CAR(cur), env, quosured));

    SEXP nxt = CDR(cur);
    nxt = replace_triple_bang(nxt, cur);
    if (is_rlang_call(CAR(nxt), is_splice_sym)) {
      if (quosured) {
        cur = splice_nxt(cur, nxt, env);
        cur = nxt; // Don't interpolate unquoted stuff
      } else {
        cur = splice_value_nxt(cur, nxt, env);
      }
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env, SEXP quosured) {
  if (!r_is_lang(x))
    return x;
  if (!r_is_env(env))
    r_abort("`env` must be an environment");

  x = KEEP(r_duplicate(x, false));
  x = interp_lang(x, env, r_as_bool(quosured));

  FREE(1);
  return x;
}
