#include "rlang/rlang.h"
#include "expr-interp.h"

sexp* rlang_ns_get(const char* name);
sexp* r_str_unserialise_unicode(sexp*);


static bool needs_fixup(sexp* x) {
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
static void poke_bang_bang_operand(sexp* bang, struct expansion_info* info) {
  sexp* fixed = bang;
  while (needs_fixup(r_node_cadr(fixed))) {
    fixed = r_node_cadr(fixed);
  }

  info->parent = r_node_cdr(fixed);
  if (bang != fixed) {
    info->root = r_node_cadr(bang);
  }
  info->operand = r_node_cadr(fixed);
}

struct expansion_info which_bang_op(sexp* x) {
  struct expansion_info info = init_expansion_info();

  if (!r_is_call(x, "!")) {
    return info;
  }

  sexp* second = r_node_cadr(x);
  if (!r_is_call(second, "!")) {
    return info;
  }

  sexp* third = r_node_cadr(second);

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

struct expansion_info which_expansion_op(sexp* x, bool unquote_names) {
  struct expansion_info info = which_bang_op(x);

  if (r_typeof(x) != r_type_call) {
    return info;
  }
  if (info.op) {
    return info;
  }

  if (unquote_names && r_is_call(x, ":=")) {
    info.op = OP_EXPAND_UQN;
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

struct expansion_info which_node_expansion_op(sexp* x, bool unquote_names) {
  struct expansion_info info = which_bang_op(x);

  if (info.op == OP_EXPAND_UQS) {
    return info;
  }

  if (is_splice_call(x)) {
    info.op = OP_EXPAND_UQS;
    info.operand = r_node_cadr(x);
  } else if (unquote_names && r_is_call(x, ":=")) {
    info.op = OP_EXPAND_UQN;
    return info;
  }

  return info;
}


static sexp* bang_bang_teardown(sexp* value, struct expansion_info info) {
  if (info.parent != r_null) {
    r_node_poke_car(info.parent, value);
  }

  if (info.root == r_null) {
    return value;
  } else {
    return info.root;
  }
}

static sexp* bang_bang(struct expansion_info info, sexp* env) {
  sexp* value = r_eval(info.operand, env);
  return bang_bang_teardown(value, info);
}
static sexp* bang_bang_expression(struct expansion_info info, sexp* env) {
  sexp* value = r_eval(info.operand, env);

  if (r_is_formulaish(value, -1, 0)) {
    value = r_get_expression(value, NULL);
  }

  return bang_bang_teardown(value, info);
}

sexp* big_bang_coerce(sexp* expr) {
  switch (r_typeof(expr)) {
  case r_type_null:
    return expr;
  case r_type_pairlist:
    return r_duplicate(expr, true);
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
  case r_type_list:
    return r_vec_coerce(expr, r_type_pairlist);
  case r_type_call:
    if (r_is_symbol(r_node_car(expr), "{")) {
      return r_node_cdr(expr);
    }
    // else fallthrough
  default:
    return r_new_node(expr, r_null);
  }
}

sexp* big_bang(sexp* operand, sexp* env, sexp* node, sexp* next) {
  sexp* value = KEEP(r_eval(operand, env));
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

sexp* def_unquote_name(sexp* expr, sexp* env) {
  int n_kept = 0;
  sexp* lhs = r_node_cadr(expr);

  struct expansion_info info = which_expansion_op(lhs, true);

  switch (info.op) {
  case OP_EXPAND_NONE:
    break;
  case OP_EXPAND_UQ:
    lhs = KEEP_N(r_eval(info.operand, env), &n_kept);
    break;
  case OP_EXPAND_UQE:
    r_abort("The LHS of `:=` can't be unquoted with `UQE()`");
  case OP_EXPAND_UQS:
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
  case OP_EXPAND_UQN:
    r_abort("Internal error: Chained `:=` should have been detected earlier");
  }

  int err = 0;
  lhs = r_new_symbol(lhs, &err);
  if (err) {
    r_abort("The LHS of `:=` must be a string or a symbol");
  }

  sexp* name = r_sym_str(lhs);

  // Unserialise unicode points such as <U+xxx> that arise when
  // UTF-8 names are converted to symbols and the native encoding
  // does not support the characters (i.e. all the time on Windows)
  name = r_str_unserialise_unicode(name);

  FREE(n_kept);
  return name;
}


static sexp* node_list_interp(sexp* x, sexp* env, bool unquote_names);

sexp* call_interp(sexp* x, sexp* env, bool unquote_names)  {
  struct expansion_info info = which_expansion_op(x, unquote_names);
  return call_interp_impl(x, env, info, unquote_names);
}

sexp* call_interp_impl(sexp* x, sexp* env,
                       struct expansion_info info,
                       bool unquote_names) {
  if (info.op && r_node_cdr(x) == r_null) {
    r_abort("`UQ()`, `UQE()` and `UQS()` must be called with an argument");
  }

  switch (info.op) {
  case OP_EXPAND_UQN:
    if (unquote_names) {
      r_abort("Can't use `:=` at top level");
    }
    // else fallthrough
  case OP_EXPAND_NONE:
    if (r_typeof(x) == r_type_call) {
      return node_list_interp(x, env, unquote_names);
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

static sexp* node_list_interp(sexp* x, sexp* env, bool unquote_names) {
  for (sexp* node = x; node != r_null; node = r_node_cdr(node)) {
    r_node_poke_car(node, call_interp(r_node_car(node), env, unquote_names));

    sexp* next = r_node_cdr(node);
    sexp* next_head = r_node_car(next);

    struct expansion_info info;
    info = which_node_expansion_op(next_head, unquote_names);

    switch (info.op) {
    case OP_EXPAND_UQS:
      node = big_bang(info.operand, env, node, next);
      break;
    case OP_EXPAND_UQN: {
      sexp* name = def_unquote_name(next_head, env);
      r_node_poke_tag(next, r_str_sym(name));
      r_node_poke_car(next, r_node_cadr(r_node_cdr(next_head)));

      if (r_is_call(r_node_car(next), ":=")) {
        r_abort("`:=` can't be chained");
      }

      break;
    }
    default:
      break;
    }
  }

  return x;
}

sexp* rlang_interp(sexp* x, sexp* env, sexp* unquote_names) {
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }
  if (r_typeof(x) != r_type_call) {
    return x;
  }

  x = KEEP(r_duplicate(x, false));
  x = call_interp(x, env, unquote_names);

  FREE(1);
  return x;
}
