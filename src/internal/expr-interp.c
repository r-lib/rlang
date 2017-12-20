#include <rlang.h>
#include "expr-interp.h"
#include "utils.h"


bool expr_maybe_needs_fixup(sexp* x);

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
    if (expr_maybe_needs_fixup(third)) {
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

void signal_uq_soft_deprecation() {
  signal_soft_deprecation(
    "`UQ()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!` instead."
  );
}
void signal_uqs_soft_deprecation() {
  signal_soft_deprecation(
    "`UQS()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!!` instead."
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

  if (is_maybe_rlang_call(x, "UQS")) {
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

  if (expr_maybe_needs_fixup(x)) {
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

    if (!r_is_namespaced_call(x, "rlang", NULL)) {
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


static bool op_needs_fixup(enum r_operator op) {
  switch (op) {
  case R_OP_GREATER:
  case R_OP_GREATER_EQUAL:
  case R_OP_LESS:
  case R_OP_LESS_EQUAL:
  case R_OP_EQUAL:
  case R_OP_NOT_EQUAL:
  case R_OP_PLUS:
  case R_OP_MINUS:
  case R_OP_TIMES:
  case R_OP_RATIO:
  case R_OP_MODULO:
  case R_OP_SPECIAL:
  case R_OP_COLON1:
    return true;
  default:
    return false;
  }
}
bool expr_maybe_needs_fixup(sexp* x) {
  return op_needs_fixup(r_which_operator(x));
}

static bool op_has_precedence(enum r_operator x, enum r_operator y) {
  if (x == R_OP_NONE || x > R_OP_MAX || y > R_OP_MAX) {
    r_abort("Internal error: `enum r_operator` out of bounds");
  }

  struct r_op_binding_power x_info = r_ops_binding_powers[x];
  struct r_op_binding_power y_info = r_ops_binding_powers[y];

  if (x_info.delimited) {
    return true;
  }
  if (y_info.delimited) {
    return false;
  }

  uint8_t x_power = x_info.power;
  uint8_t y_power = y_info.power;

  if (x_power == y_power) {
    return r_ops_binding_powers[x].assoc == -1;
  } else {
    return x_power > y_power;
  }
}
bool call_has_precedence(sexp* x, sexp* y) {
  return op_has_precedence(r_which_operator(x), r_which_operator(y));
}

struct ast_rotation_info {
  enum r_operator upper_pivot_op;
  sexp* upper_pivot;
  sexp* lower_pivot;
  sexp* lower_root;
  sexp* target;
};

// Defined below
static sexp* fixup_interp(sexp* x, sexp* env);
static sexp* fixup_interp_first(sexp* x, sexp* env);
static sexp* node_list_interp(sexp* x, sexp* env);
static sexp* node_list_interp_fixup(sexp* x, sexp* env,
                                    struct ast_rotation_info* rotation_info);

static sexp* maybe_rotate(sexp* root, sexp* env, struct ast_rotation_info* info) {
  if (info->upper_pivot_op == R_OP_NONE) {
    return root;
  }

  // If rotation is not needed expand the RHS normally
  if (!op_has_precedence(r_which_operator(root), info->upper_pivot_op)) {
    sexp* rhs_node = r_node_cddr(root);
    r_node_poke_car(rhs_node, call_interp(r_node_car(rhs_node), env));
    return root;
  }

  // Swap the lower root's RHS with the lower pivot's LHS
  r_node_poke_car(r_node_cddr(info->lower_root), r_node_cadr(info->lower_pivot));
  r_node_poke_cadr(info->lower_pivot, root);

  // After rotation the upper pivot is the new root
  root = info->upper_pivot;

  // Reset info to prevent rotating multiple times
  info->upper_pivot_op = R_OP_NONE;
  info->upper_pivot = NULL;
  info->lower_pivot = NULL;
  info->lower_root = NULL;
  info->target = NULL;

  // Recurse on the RHS of the new root
  // FIXME: Don't expand LHS?
  node_list_interp_fixup(root, env, info);
  return maybe_rotate(root, env, info);
}


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
}

// If !! is the root expression there is no rotation needed. Just
// unquote the leftmost child across problematic binary operators.
// However the resulting root might be involved in a rotation for
// a subsequent !! call.
static sexp* fixup_interp_first(sexp* x, sexp* env) {
  sexp* parent = NULL; // `parent` will always be initialised in the loop
  sexp* target = x;
  while (expr_maybe_needs_fixup((parent = target, target = r_node_cadr(target))));

  // Unquote target
  r_node_poke_cadr(parent, r_eval(target, env));

  // FIXME: Might be ok to leave left child alone when recursing?
  return fixup_interp(x, env);
}

// Expression is an operator that might need changes in the AST if
// we find a !! call down the line. I.e. it is an operator whose
// precedence is between prec(`!`) and prec(`!!`).
static sexp* fixup_interp(sexp* x, sexp* env) {
  struct ast_rotation_info rotation_info = {
    .upper_pivot_op = R_OP_NONE,
    .lower_pivot = NULL,
    .upper_pivot = NULL,
    .lower_root = NULL,
    .target = NULL
  };

  // Look for problematic !! calls and expand arguments on the way.
  // If a pivot is found rotate it around `x`.
  node_list_interp_fixup(x, env, &rotation_info);
  return maybe_rotate(x, env, &rotation_info);
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

static sexp* find_upper_pivot(sexp* x, struct ast_rotation_info* info) {
  if (!r_is_call(x, "!")) {
    return NULL;
  }

  x = r_node_cadr(x);
  if (!r_is_call(x, "!")) {
    return NULL;
  }

  x = r_node_cadr(x);
  if (r_is_call(x, "!")) {
    return NULL;
  }

  enum r_operator op = r_which_operator(x);
  if (!op_needs_fixup(op)) {
    return  NULL;
  }

  info->upper_pivot_op = op;
  info->upper_pivot = x;
  return x;
}

/**
 *
 * Climb through LHS's until we find an operator that has greater
 * precendence than the upper pivot. This node is the pivot and will
 * be reattached to the root. Expand all RHS's as we go.
 *
 */
static void find_lower_pivot(sexp* node, sexp* env,
                             struct ast_rotation_info* info) {
  // Only expand RHS if not the upper pivot because there might be
  // consecutive rotations needed. The upper pivot's RHS will be
  // expanded after the current rotation is complete.
  if (node != info->upper_pivot) {
    sexp* rhs_node = r_node_cddr(node);
    r_node_poke_car(rhs_node, call_interp(r_node_car(rhs_node), env));
  }

  sexp* lhs = r_node_cadr(node);
  enum r_operator lhs_op = r_which_operator(lhs);

  if (!op_needs_fixup(lhs_op)) {
    if (!info->lower_pivot) {
      info->lower_pivot = node;
    }

    sexp* target = r_eval(lhs, env);
    r_node_poke_cadr(node, target);

    // Stop recursion as we found both target and lower pivot
    return;
  }

  if (!op_has_precedence(info->upper_pivot_op, lhs_op)) {
    info->lower_pivot = node;
  }

  // Recurse
  find_lower_pivot(lhs, env, info);
}

/**
 *
 * Takes a call to a binary operator whose precedence is between
 * prec(`!`) and prec(`!!`).
 *
 * - Somehow, root-swap parameter
 * - Normal interp on LHS
 * - Interp RHS with potential root swap
 * - If we get a root-swap, compare prec of pivot to prec of current
 *
 */
static sexp* node_list_interp_fixup(sexp* x, sexp* env,
                                    struct ast_rotation_info* rotation_info) {
  sexp* lhs_node = r_node_cdr(x);
  sexp* rhs_node = r_node_cddr(x);
  sexp* lhs = r_node_car(lhs_node);
  sexp* rhs = r_node_car(rhs_node);

  // Expand the LHS normally, it never needs changes in the AST
  struct expansion_info info = is_big_bang_op(lhs);
  if (info.op == OP_EXPAND_UQS) {
    sexp* node = big_bang(info.operand, env, lhs_node, rhs_node);
    rhs_node = r_node_cdr(node);
  } else {
    r_node_poke_car(lhs_node, call_interp(r_node_car(lhs_node), env));
  }


  // An upper pivot is an operand of a !! call that is a binary
  // operation whose precedence is problematic (between prec(`!`) and
  // prec(`!!`))
  sexp* upper_pivot = find_upper_pivot(rhs, rotation_info);
  if (upper_pivot) {
    rotation_info->lower_root = x;

    // Reattach the RHS to the upper pivot stripped of its !! call
    // in case there is no rotation around the lower root
    r_node_poke_car(rhs_node, upper_pivot);

    // There might be a lower pivot, so we need to find it. Also find
    // the target of unquoting (leftmost leaf whose predecence is
    // greater than prec(`!!`)) and unquote it.
    find_lower_pivot(upper_pivot, env, rotation_info);

    return x;
  }

  // If `rhs` is an operator that might be involved in a rotation
  // recurse with the fixup version
  if (expr_maybe_needs_fixup(rhs)) {
    node_list_interp_fixup(rhs, env, rotation_info);
    return x;
  }

  // RHS is not a binary operation that might need changes in the AST
  // so expand it as usual
  r_node_poke_car(rhs_node, call_interp(rhs, env));
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
