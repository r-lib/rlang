#include <rlang.h>
#include "expr-interp.h"
#include "expr-interp-rotate.h"
#include "utils.h"


struct expansion_info which_bang_op(sexp* first) {
  struct expansion_info info = init_expansion_info();

  if (r_is_call(first, "(")) {
    sexp* paren = r_node_cadr(first);
    if (r_is_call(paren, "(")) {
      return info;
    }

    struct expansion_info inner_info = which_bang_op(paren);

    // Check that `root` is NULL so we don't remove parentheses when
    // there's an operation tail (i.e. when the parse tree was fixed
    // up to bind tightly)
    if (inner_info.op == OP_EXPAND_UQ && inner_info.root == r_null) {
      return inner_info;
    } else {
      return info;
    }
  }

  if (!r_is_call(first, "!")) {
    return info;
  }

  sexp* second = r_node_cadr(first);
  if (!r_is_call(second, "!")) {
    return info;
  }

  sexp* third = r_node_cadr(second);

  // Need to fill in `info` for `!!` because parse tree might need changes
  if (!r_is_call(third, "!")) {
    if (is_problematic_op(third)) {
      info.op = OP_EXPAND_FIXUP;
      info.operand = third;
    } else {
      info.op = OP_EXPAND_UQ;
      info.parent = r_node_cdr(second);
      info.operand = third;
    }
    return info;
  }

  info.op = OP_EXPAND_UQS;
  info.operand = r_node_cadr(third);
  return info;
}

// These functions are questioning and might be soft-deprecated in the
// future
void signal_uq_soft_deprecation() {
  return ;
  signal_soft_deprecation(
    "`UQ()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!` instead."
  );
}
void signal_uqs_soft_deprecation() {
  return ;
  signal_soft_deprecation(
    "`UQS()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!!` instead."
  );
}

void signal_namespaced_uq_deprecation() {
  signal_soft_deprecation(
    "Prefixing `UQ()` with a namespace is soft-deprecated as of rlang 0.2.0. "
    "Please use the unprefixed form instead."
  );
}
void signal_namespaced_uqs_deprecation() {
  signal_soft_deprecation(
    "Prefixing `UQS()` with a namespace is soft-deprecated as of rlang 0.2.0. "
    "Please use the unprefixed form instead."
  );
}

void maybe_poke_big_bang_op(sexp* x, struct expansion_info* info) {
  if (r_is_call(x, "!!!")) {
    if (r_node_cddr(x) != r_null) {
      r_abort("Can't supply multiple arguments to `!!!`");
    }
    info->op = OP_EXPAND_UQS;
    info->operand = r_node_cadr(x);
    return ;
  }

  // Handle expressions like foo::`!!`(bar) or foo$`!!`(bar)
  if (r_is_prefixed_call(x, "!!!")) {
    const char* name = r_sym_c_str(r_node_caar(x));
    r_abort("Prefix form of `!!!` can't be used with `%s`", name);
  }

  bool namespaced_uqs = r_is_namespaced_call(x, "rlang", "UQS");
  if (namespaced_uqs) {
    signal_namespaced_uqs_deprecation();
  }
  if (namespaced_uqs || r_is_call(x, "UQS")) {
    signal_uqs_soft_deprecation();
    info->op = OP_EXPAND_UQS;
    info->operand = r_node_cadr(x);
    return ;
  }
}

struct expansion_info which_expansion_op(sexp* x, bool unquote_names) {
  struct expansion_info info = which_bang_op(x);

  if (r_typeof(x) != r_type_call) {
    return info;
  }
  if (info.op) {
    return info;
  }

  if (is_problematic_op(x)) {
    info.op = OP_EXPAND_FIXUP;
    return info;
  }

  if (unquote_names && r_is_call(x, ":=")) {
    info.op = OP_EXPAND_UQN;
    return info;
  }


  if (r_is_call(x, "!!")) {
    info.op = OP_EXPAND_UQ;
    info.operand = r_node_cadr(x);
    return info;
  }

  // Handle expressions like foo::`!!`(bar) or foo$`!!`(bar)
  if (r_is_prefixed_call(x, "!!")) {
    info.op = OP_EXPAND_UQ;
    info.operand = r_node_cadr(x);
    info.parent = r_node_cdr(r_node_cdar(x));
    info.root = r_node_car(x);
    return info;
  }

  maybe_poke_big_bang_op(x, &info);
  if (info.op == OP_EXPAND_UQS) {
    return info;
  }

  // This logic is complicated because rlang::UQ() gets fully unquoted
  // but not foobar::UQ(). The functional form UI is now retired so
  // we'll be able to simplify this in the future.
  if (r_is_prefixed_call(x, "UQ")) {
    signal_uq_soft_deprecation();

    info.op = OP_EXPAND_UQ;
    info.operand = r_node_cadr(x);

    if (r_is_namespaced_call(x, "rlang", NULL)) {
      signal_namespaced_uq_deprecation();
    } else {
      info.parent = r_node_cdr(r_node_cdar(x));
      info.root = r_node_car(x);
    }

    return info;
  }
  if (r_is_call(x, "UQ")) {
    signal_uq_soft_deprecation();
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

  return info;
}

struct expansion_info is_big_bang_op(sexp* x) {
  struct expansion_info info = which_bang_op(x);

  if (info.op != OP_EXPAND_UQS) {
    maybe_poke_big_bang_op(x, &info);
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
  sexp* value = KEEP(r_eval(info.operand, env));

  if (r_is_formulaish(value, -1, 0)) {
    value = rlang_get_expression(value, NULL);
  }
  value = bang_bang_teardown(value, info);

  FREE(1);
  return value;
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


// Defined below
static sexp* node_list_interp(sexp* x, sexp* env);

sexp* call_interp(sexp* x, sexp* env)  {
  struct expansion_info info = which_expansion_op(x, false);
  return call_interp_impl(x, env, info);
}

sexp* call_interp_impl(sexp* x, sexp* env, struct expansion_info info) {
  if (info.op && r_node_cdr(x) == r_null) {
    r_abort("`UQ()`, `UQE()` and `UQS()` must be called with an argument");
  }
  if (info.op == OP_EXPAND_UQE) {
    r_warn("`UQE()` is deprecated. Please use `!! get_expr(x)`");
  }

  switch (info.op) {
  case OP_EXPAND_NONE:
    if (r_typeof(x) != r_type_call) {
      return x;
    } else {
      return node_list_interp(x, env);
    }
  case OP_EXPAND_UQ:
    return bang_bang(info, env);
  case OP_EXPAND_UQE:
    return bang_bang_expression(info, env);
  case OP_EXPAND_FIXUP:
    if (info.operand == r_null) {
      return fixup_interp(x, env);
    } else {
      return fixup_interp_first(info.operand, env);
    }
  case OP_EXPAND_UQS:
    r_abort("Can't use `!!!` at top level");
  case OP_EXPAND_UQN:
    r_abort("Internal error: Deep `:=` unquoting");
  }

  // Silence mistaken noreturn warning on GCC
  r_abort("Never reached");
}

static sexp* node_list_interp(sexp* x, sexp* env) {
  for (sexp* node = x; node != r_null; node = r_node_cdr(node)) {
    r_node_poke_car(node, call_interp(r_node_car(node), env));

    sexp* next = r_node_cdr(node);
    sexp* next_head = r_node_car(next);

    struct expansion_info info = is_big_bang_op(next_head);
    if (info.op == OP_EXPAND_UQS) {
      node = big_bang(info.operand, env, node, next);
    }
  }

  return x;
}

sexp* rlang_interp(sexp* x, sexp* env) {
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
