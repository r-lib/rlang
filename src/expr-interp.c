#include "rlang/rlang.h"
#include "expr-interp.h"

SEXP rlang_ns_get(const char* name);
SEXP r_str_unserialise_unicode(SEXP);

static SEXP interp_lang_node(SEXP x, SEXP env);
static SEXP uqs_fun;


int bang_level(SEXP x, SEXP* operand) {
  if (!r_is_call(x, "!")) {
    return 0;
  }

  SEXP arg = r_node_cadr(x);
  if (!r_is_call(arg, "!")) {
    return 0;
  }

  arg = r_node_cadr(arg);
  if (!r_is_call(arg, "!")) {
    if (operand) {
      *operand = arg;
    }
    return 2;
  }

  if (operand) {
    *operand = r_node_cadr(arg);
  }
  return 3;
}

int which_expand_op(SEXP x, SEXP* operand) {
  int level = bang_level(x, operand);
  if (level == 2) {
    return OP_EXPAND_UQ;
  } else if (level == 3) {
    return OP_EXPAND_UQS;
  }

  if (is_rlang_call_any(x, uq_names, UQ_N)) {
    if (operand) {
      *operand = r_node_cadr(x);
    }
    return OP_EXPAND_UQ;
  }

  if (is_splice_call(x)) {
    if (operand) {
      *operand = r_node_cadr(x);
    }
    return OP_EXPAND_UQS;
  }

  return OP_EXPAND_NONE;
}


static inline bool needs_fixup(SEXP x) {
  if (r_is_call_any(x, fixup_ops_names, FIXUP_OPS_N)) {
    return true;
  }

  // Don't fixup unary operators
  if (r_is_call_any(x, fixup_unary_ops_names, FIXUP_UNARY_OPS_N)) {
    return r_node_cddr(x) != r_null;
  }

  if (r_is_special_op_call(x)) {
    return true;
  }

  return false;
}

static SEXP uq_call(SEXP x) {
  SEXP args = KEEP(r_new_node_list(x));
  SEXP call = r_new_call_node(r_sym("UQ"), args);
  FREE(1);
  return call;
}
static SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x, NULL);
  if (bang == 3 || r_is_maybe_prefixed_call_any(x, uqs_names, UQS_N)) {
    r_abort("Can't splice at top-level");
  }
  if (bang != 2) {
    return x;
  }

  SEXP cadr = r_node_cadr(x);
  SEXP cadr_cadr = r_node_cadr(r_node_cadr(x));

  if (!needs_fixup(cadr_cadr)) {
    x = cadr;
    r_node_poke_car(x, r_sym("UQ"));
    return x;
  }

  // We have an infix expression. Fix up AST so that `!!` binds tightly
  x = cadr_cadr;

  SEXP innermost = x;
  while (needs_fixup(r_node_cadr(innermost))) {
    innermost = r_node_cadr(innermost);
  }

  r_node_poke_cadr(innermost, uq_call(r_node_cadr(innermost)));
  return x;
}
static SEXP replace_triple_bang(SEXP x) {
  SEXP node = r_node_cadr(r_node_cadr(x));

  r_node_poke_car(node, r_sym("UQS"));

  return node;
}

static void unquote_check(SEXP x) {
  if (r_node_cdr(x) == r_null)
    r_abort("`UQ()` must be called with an argument");
}

static SEXP unquote(SEXP x, SEXP env, SEXP uq_sym) {
  if (r_is_symbol(uq_sym, "!!")) {
    uq_sym = r_sym("UQE");
  }

  // Inline unquote function before evaluation because even `::` might
  // not be available in interpolation environment.
  SEXP uq_fun = KEEP(r_env_get(r_ns_env("rlang"), uq_sym));
  uq_fun = KEEP(Rf_lang2(uq_fun, x));

  SEXP unquoted = KEEP(r_eval(uq_fun, env));

  FREE(3);
  return unquoted;
}

static SEXP unquote_prefixed_uq(SEXP x, SEXP env) {
  SEXP uq_sym = r_node_cadr(r_node_cdar(x));
  SEXP unquoted = KEEP(unquote(r_node_cadr(x), env, uq_sym));
  r_node_poke_cdr(r_node_cdar(x), r_new_node(unquoted, r_null));
  FREE(1);

  if (r_is_namespaced_call(x, "rlang")) {
    x = r_node_cadr(r_node_cdar(x));
  } else {
    x = r_node_car(x);
  }
  return x;
}

static SEXP splice_next(SEXP node, SEXP next, SEXP env) {
  r_node_poke_car(r_node_car(next), uqs_fun);

  // UQS() does error checking and returns a pair list
  SEXP spliced_node = KEEP(r_eval(r_node_car(next), env));

  if (spliced_node == r_null) {
    r_node_poke_cdr(node, r_node_cdr(next));
  } else {
    // Insert spliced_node into existing pairlist of args
    r_node_poke_cdr(r_node_tail(spliced_node), r_node_cdr(next));
    r_node_poke_cdr(node, spliced_node);
  }

  FREE(1);
  return next;
}

SEXP interp_lang(SEXP x, SEXP env)  {
  if (!uqs_fun) {
    uqs_fun = rlang_ns_get("UQS");
  }
  if (r_kind(x) != LANGSXP) {
    return x;
  }

  KEEP(x);
  x = replace_double_bang(x);

  if (r_is_prefixed_call_any(x, uq_names, UQ_N)) {
    unquote_check(x);
    x = unquote_prefixed_uq(x, env);
  } else if (r_is_call_any(x, uq_names, UQ_N)) {
    unquote_check(x);
    SEXP uq_sym = r_node_car(x);
    x = unquote(r_node_cadr(x), env, uq_sym);
  } else {
    x = interp_lang_node(x, env);
  }

  FREE(1);
  return x;
}

static SEXP interp_lang_node(SEXP x, SEXP env) {
  SEXP node, next, next_head;

  for (node = x; node != r_null; node = r_node_cdr(node)) {
    r_node_poke_car(node, interp_lang(r_node_car(node), env));

    next = r_node_cdr(node);
    next_head = r_node_car(next);

    // FIXME double check
    if (bang_level(next_head, NULL) == 3) {
      next_head = replace_triple_bang(next_head);
      r_node_poke_car(next, next_head);
    }
    if (is_splice_call(next_head)) {
      node = splice_next(node, next, env);
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env) {
  if (r_kind(x) != LANGSXP) {
    return x;
  }
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }

  x = KEEP(r_duplicate(x, false));
  x = interp_lang(x, env);

  FREE(1);
  return x;
}
