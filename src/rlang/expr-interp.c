#include "rlang.h"

static
SEXP interp_lang(SEXP x, SEXP env, bool quosured);
static
SEXP interp_lang_node(SEXP x, SEXP env, bool quosured);


static
int bang_level(SEXP x) {
  if (!is_lang(x, "!"))
    return 0;

  SEXP arg = r_node_cdr(x);
  if (r_kind(arg) == NILSXP || !is_lang(r_node_car(arg), "!"))
    return 1;

  arg = r_node_cdr(r_node_car(arg));
  if (r_kind(arg) == NILSXP || !is_lang(r_node_car(arg), "!"))
    return 2;

  return 3;
}

static
int is_uq_sym(SEXP x) {
  if (r_kind(x) != SYMSXP)
    return 0;
  else
    return r_is_symbol(x, "UQ") || r_is_symbol(x, "UQE") || r_is_symbol(x, "!!");
}
static
int is_splice_sym(SEXP x) {
  if (r_kind(x) != SYMSXP)
    return 0;
  else
    return r_is_symbol(x, "UQS") || r_is_symbol(x, "!!!");
}

static
SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x);
  if (bang == 3 || is_any_call(x, is_splice_sym)){
    r_abort("Can't splice at top-level");
  } else if (bang == 2) {
    x = r_node_cadr(x);
    r_node_poke_car(x, r_sym("UQ"));
  }
  return x;
}
static
SEXP replace_triple_bang(SEXP x) {
  if (bang_level(r_node_car(x)) != 3) {
    return x;
  }

  SEXP node = r_node_cdar(r_node_cdar(x));

  r_node_poke_car(r_node_car(node), r_sym("UQS"));
  r_node_poke_cdr(node, r_node_cdr(x));

  return node;
}

static
void unquote_check(SEXP x) {
  if (r_node_cdr(x) == R_NilValue)
    r_abort("`UQ()` must be called with an argument");
}

static
SEXP unquote(SEXP x, SEXP env, SEXP uq_sym, bool quosured) {
  if (r_is_symbol(uq_sym, "!!"))
    uq_sym = r_sym("UQE");

  // Inline unquote function before evaluation because even `::` might
  // not be available in interpolation environment.
  SEXP uq_fun = r_env_get(r_ns_env("rlang"), uq_sym);

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(uq_fun, &ipx);
  REPROTECT(uq_fun = Rf_lang2(uq_fun, x), ipx);

  SEXP unquoted;
  REPROTECT(unquoted = r_eval(uq_fun, env), ipx);

  if (!quosured && is_symbolic(unquoted))
    unquoted = Rf_lang2(r_sym("quote"), unquoted);

  FREE(1);
  return unquoted;
}

static
SEXP unquote_prefixed_uq(SEXP x, SEXP env, bool quosured) {
  SEXP uq_sym = r_node_cadr(r_node_cdar(x));
  SEXP unquoted = KEEP(unquote(r_node_cadr(x), env, uq_sym, quosured));
  r_node_poke_cdr(r_node_cdar(x), r_new_node_(unquoted, R_NilValue));
  FREE(1);

  if (is_rlang_prefixed(x, NULL))
    x = r_node_cadr(r_node_cdar(x));
  else
    x = r_node_car(x);
  return x;
}

static
SEXP splice_next(SEXP node, SEXP next, SEXP env) {
  static SEXP uqs_fun;
  if (!uqs_fun)
    uqs_fun = rlang_obj("UQS");
  r_node_poke_car(r_node_car(next), uqs_fun);

  // UQS() does error checking and returns a pair list
  SEXP spliced_node = KEEP(r_eval(r_node_car(next), env));

  if (spliced_node == R_NilValue) {
    r_node_poke_cdr(node, r_node_cdr(next));
  } else {
    // Insert spliced_node into existing pairlist of args
    r_node_poke_cdr(r_node_tail(spliced_node), r_node_cdr(next));
    r_node_poke_cdr(node, spliced_node);
  }

  FREE(1);
  return next;
}

static
SEXP value_splice_next(SEXP cur, SEXP nxt, SEXP env) {
  r_node_poke_car(r_node_car(nxt), rlang_obj("splice"));
  r_node_poke_car(nxt, r_eval(r_node_car(nxt), env));
  return cur;
}

static
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
    SEXP uq_sym = r_node_car(x);
    x = unquote(r_node_cadr(x), env, uq_sym, quosured);
  } else {
    x = interp_lang_node(x, env, quosured);
  }

  FREE(1);
  return x;
}

static
bool is_splice_node(SEXP node) {
  return is_rlang_call(r_node_car(node), is_splice_sym);
}

static
SEXP interp_lang_node(SEXP x, SEXP env, bool quosured) {
  SEXP node, next;

  for (node = x; node != R_NilValue; node = r_node_cdr(node)) {
    r_node_poke_car(node, interp_lang(r_node_car(node), env, quosured));

    next = r_node_cdr(node);
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
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }

  x = KEEP(r_duplicate(x, false));
  x = interp_lang(x, env, r_as_bool(quosured));

  FREE(1);
  return x;
}
