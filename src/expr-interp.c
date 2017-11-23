#include "rlang/rlang.h"
#include "expr-interp.h"

SEXP rlang_ns_get(const char* name);
SEXP r_str_unserialise_unicode(SEXP);


static bool needs_fixup(SEXP x) {
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

// Takes the trailing `!`
static void poke_bang_bang_operand(SEXP bang, struct expansion_info* info) {
  SEXP fixed = bang;
  while (needs_fixup(r_node_cadr(fixed))) {
    fixed = r_node_cadr(fixed);
  }

  info->parent = r_node_cdr(fixed);
  if (bang != fixed) {
    info->root = r_node_cadr(bang);
  }
  info->operand = r_node_cadr(fixed);
}

struct expansion_info which_bang_op(SEXP x) {
  struct expansion_info info = init_expansion_info();

  if (!r_is_call(x, "!")) {
    return info;
  }

  SEXP second = r_node_cadr(x);
  if (!r_is_call(second, "!")) {
    return info;
  }

  SEXP third = r_node_cadr(second);

  // Need to fill in `info` for `!!` because parse tree might need changes
  if (!r_is_call(third, "!")) {
    info.op = OP_EXPAND_UQ;
    poke_bang_bang_operand(second, &info);
    return info;
  }

  info.op = OP_EXPAND_UQS;
  info.operand = r_node_cadr(third);
  return info;
}

struct expansion_info which_expansion_op(SEXP x) {
  struct expansion_info info = which_bang_op(x);

  if (r_typeof(x) != r_type_call) {
    return info;
  }
  if (info.op) {
    return info;
  }

  // This logic is complicated because rlang::UQ() gets fully unquoted
  // but not foobar::UQ(). The functional form UI is a design mistake.

  if (r_is_prefixed_call(x, "UQ")) {
    info.op = OP_EXPAND_UQ;
    info.operand = r_node_cadr(x);

    if (!r_is_namespaced_call(x, "rlang", NULL)) {
      info.parent = r_node_cdr(r_node_cdar(x));
      info.root = r_node_car(x);
    }

    return info;
  }
  if (r_is_call(x, "UQ")) {
    info.op = OP_EXPAND_UQ;
    info.operand = r_node_cadr(x);
    return info;
  }


  if (r_is_prefixed_call_any(x, uqe_names, UQE_N)) {
    info.op = OP_EXPAND_UQE;
    info.operand = r_node_cadr(x);

    if (!r_is_namespaced_call(x, "rlang", NULL)) {
      info.parent = r_node_cdr(r_node_cdar(x));
      info.root = r_node_car(x);
    }

    return info;
  }
  if (r_is_call_any(x, uqe_names, UQE_N)) {
    info.op = OP_EXPAND_UQE;
    info.operand = r_node_cadr(x);
    return info;
  }


  if (is_splice_call(x)) {
    info.op = OP_EXPAND_UQS;
    info.operand = r_node_cadr(x);
    return info;
  }

  return info;
}

struct expansion_info is_big_bang_op(SEXP x) {
  struct expansion_info info = which_bang_op(x);

  if (info.op == OP_EXPAND_UQS) {
    return info;
  }

  if (is_splice_call(x)) {
    info.op = OP_EXPAND_UQS;
    info.operand = r_node_cadr(x);
  }

  return info;
}


static SEXP bang_bang_teardown(SEXP value, struct expansion_info info) {
  if (info.parent != r_null) {
    r_node_poke_car(info.parent, value);
  }

  if (info.root == r_null) {
    return value;
  } else {
    return info.root;
  }
}

static SEXP bang_bang(struct expansion_info info, SEXP env) {
  SEXP value = r_eval(info.operand, env);
  return bang_bang_teardown(value, info);
}
static SEXP bang_bang_expression(struct expansion_info info, SEXP env) {
  SEXP value = r_eval(info.operand, env);

  if (r_is_formulaish(value, -1, 0)) {
    value = r_get_expression(value, NULL);
  }

  return bang_bang_teardown(value, info);
}

SEXP big_bang_coerce(SEXP expr) {
  switch (r_typeof(expr)) {
  case r_type_null:
  case r_type_pairlist:
    return expr;
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
  case r_type_list: {
    static SEXP coercer = NULL;
    if (!coercer) { coercer = r_base_ns_get("as.pairlist"); }
    SEXP coerce_args = KEEP(r_new_node(expr, r_null));
    SEXP coerce_call = KEEP(r_new_call_node(coercer, coerce_args));
    SEXP coerced = r_eval(coerce_call, r_empty_env);
    FREE(2);
    return coerced;
  }
  case r_type_call:
    if (r_is_symbol(r_node_car(expr), "{")) {
      return r_node_cdr(expr);
    }
    // else fallthrough
  default:
    r_abort("`!!!` expects a vector, a node list, or a call to `{`");
  }
}

SEXP big_bang(SEXP operand, SEXP env, SEXP node, SEXP next) {
  SEXP value = KEEP(r_eval(operand, env));
  value = big_bang_coerce(value);

  if (value == r_null) {
    r_node_poke_cdr(node, r_node_cdr(next));
  } else {
    // Insert coerced value into existing pairlist of args
    r_node_poke_cdr(r_node_tail(value), r_node_cdr(next));
    r_node_poke_cdr(node, value);
  }

  FREE(1);
  return next;
}


static SEXP node_list_interp(SEXP x, SEXP env);

SEXP call_interp(SEXP x, SEXP env)  {
  struct expansion_info info = which_expansion_op(x);
  return call_interp_impl(x, env, info);
}

SEXP call_interp_impl(SEXP x, SEXP env, struct expansion_info info) {
  if (info.op && r_node_cdr(x) == r_null) {
    r_abort("`UQ()`, `UQE()` and `UQS()` must be called with an argument");
  }

  switch (info.op) {
  case OP_EXPAND_NONE:
    if (r_typeof(x) == r_type_call) {
      return node_list_interp(x, env);
    } else {
      return x;
    }
  case OP_EXPAND_UQ:
    return bang_bang(info, env);
  case OP_EXPAND_UQE:
    return bang_bang_expression(info, env);
  case OP_EXPAND_UQS:
    r_abort("Can't use `!!!` at top level");
  }
}

static SEXP node_list_interp(SEXP x, SEXP env) {
  for (SEXP node = x; node != r_null; node = r_node_cdr(node)) {
    r_node_poke_car(node, call_interp(r_node_car(node), env));

    SEXP next = r_node_cdr(node);
    SEXP next_head = r_node_car(next);

    struct expansion_info info = is_big_bang_op(next_head);
    if (info.op == OP_EXPAND_UQS) {
      node = big_bang(info.operand, env, node, next);
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env) {
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }
  if (r_typeof(x) != r_type_call) {
    return x;
  }

  x = KEEP(r_duplicate(x, false));
  x = call_interp(x, env);

  FREE(1);
  return x;
}
