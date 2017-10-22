#include "rlang.h"

SEXP interp_lang(SEXP x, SEXP env, bool quosured);
SEXP interp_lang_node(SEXP x, SEXP env, bool quosured);


int bang_level(SEXP x) {
  if (!is_lang(x, "!"))
    return 0;

  SEXP arg = CDR(x);
  if (r_kind(arg) == NILSXP || !is_lang(CAR(arg), "!"))
    return 1;

  arg = CDR(CAR(arg));
  if (r_kind(arg) == NILSXP || !is_lang(CAR(arg), "!"))
    return 2;

  return 3;
}

int is_uq_sym(SEXP x) {
  if (r_kind(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQ") || is_sym(x, "UQE") || is_sym(x, "!!");
}
int is_splice_sym(SEXP x) {
  if (r_kind(x) != SYMSXP)
    return 0;
  else
    return is_sym(x, "UQS") || is_sym(x, "!!!");
}

SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x);
  if (bang == 3 || is_any_call(x, is_splice_sym))
    r_abort("Can't splice at top-level");
  else if (bang == 2) {
    x = CADR(x);
    SETCAR(x, Rf_install("UQ"));
  }
  return x;
}
SEXP replace_triple_bang(SEXP x) {
  if (bang_level(CAR(x)) != 3) {
    return x;
  }

  SEXP node = CDAR(CDAR(x));

  SETCAR(CAR(node), Rf_install("UQS"));
  SETCDR(node, CDR(x));

  return node;
}

void unquote_check(SEXP x) {
  if (CDR(x) == R_NilValue)
    r_abort("`UQ()` must be called with an argument");
}

bool is_bare_formula(SEXP x) {
  return r_kind(x) == LANGSXP
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
  REPROTECT(unquoted = r_eval(uq_fun, env), ipx);

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

SEXP splice_next(SEXP node, SEXP next, SEXP env) {
  static SEXP uqs_fun;
  if (!uqs_fun)
    uqs_fun = rlang_obj("UQS");
  SETCAR(CAR(next), uqs_fun);

  // UQS() does error checking and returns a pair list
  SEXP spliced_node = KEEP(r_eval(CAR(next), env));

  if (spliced_node == R_NilValue) {
    SETCDR(node, CDR(next));
  } else {
    // Insert spliced_node into existing pairlist of args
    SETCDR(r_node_tail(spliced_node), CDR(next));
    SETCDR(node, spliced_node);
  }

  FREE(1);
  return next;
}

SEXP value_splice_next(SEXP cur, SEXP nxt, SEXP env) {
  SETCAR(CAR(nxt), rlang_obj("splice"));
  SETCAR(nxt, r_eval(CAR(nxt), env));
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
    x = interp_lang_node(x, env, quosured);
  }

  FREE(1);
  return x;
}

bool is_splice_node(SEXP node) {
  return is_rlang_call(CAR(node), is_splice_sym);
}
SEXP interp_lang_node(SEXP x, SEXP env, bool quosured) {
  SEXP node, next;

  for (node = x; node != R_NilValue; node = CDR(node)) {
    SETCAR(node, interp_lang(CAR(node), env, quosured));

    next = CDR(node);
    next = replace_triple_bang(next);

    if (is_splice_node(next)) {
      if (quosured) {
        node = splice_next(node, next, env);
      } else {
        node = value_splice_next(node, next, env);
      }
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env, SEXP quosured) {
  if (!r_is_language(x)) {
    return x;
  }
  if (!r_is_env(env)) {
    r_abort("`env` must be an environment");
  }

  x = KEEP(r_duplicate(x, false));
  x = interp_lang(x, env, r_as_bool(quosured));

  FREE(1);
  return x;
}
