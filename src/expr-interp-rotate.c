#include "rlang/rlang.h"

#include "expr-interp.h"
#include "expr-interp-rotate.h"


bool op_has_precedence(enum r_operator x, enum r_operator y) {
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


struct ast_rotation_info {
  enum r_operator upper_pivot_op;
  sexp* upper_pivot;
  sexp* lower_pivot;
  sexp* upper_root;
  sexp* lower_root;
  sexp* root_parent;
  sexp* target;
};

static void initialise_rotation_info(struct ast_rotation_info* info) {
  info->upper_pivot_op = R_OP_NONE;
  info->upper_pivot = NULL;
  info->lower_pivot = NULL;
  info->upper_root = NULL;
  info->lower_root = NULL;
  info->root_parent = NULL;
  info->target = NULL;
}

// Defined below
static sexp* node_list_interp_fixup(sexp* x, sexp* env,
                                    struct ast_rotation_info* rotation_info);

static sexp* maybe_rotate(sexp* root, sexp* env, struct ast_rotation_info* info) {
  if (info->upper_pivot_op == R_OP_NONE) {
    return root;
  }

  // Rotate if needed
  if (op_has_precedence(r_which_operator(root), info->upper_pivot_op)) {
    // Swap the lower root's RHS with the lower pivot's LHS
    r_node_poke_car(r_node_cddr(info->lower_root), r_node_cadr(info->lower_pivot));
    r_node_poke_cadr(info->lower_pivot, root);

    // After rotation the upper pivot is the new root
    root = info->upper_pivot;
  } else if (info->upper_root) {
    r_node_poke_car(r_node_cddr(info->lower_root), r_node_cadr(info->lower_pivot));
    r_node_poke_cadr(info->lower_pivot, info->upper_root);
    r_node_poke_car(r_node_cddr(info->root_parent), info->upper_pivot);
  }

  // Reset info to prevent rotating multiple times
  initialise_rotation_info(info);

  // Recurse on the RHS of the new root
  // FIXME: Don't expand LHS?
  node_list_interp_fixup(root, env, info);
  return maybe_rotate(root, env, info);
}

// If !! is the root expression there is no rotation needed. Just
// unquote the leftmost child across problematic binary operators.
// However the resulting root might be involved in a rotation for
// a subsequent !! call.
sexp* fixup_interp_first(sexp* x, sexp* env) {
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
sexp* fixup_interp(sexp* x, sexp* env) {
  struct ast_rotation_info rotation_info;
  initialise_rotation_info(&rotation_info);

  // Look for problematic !! calls and expand arguments on the way.
  // If a pivot is found rotate it around `x`.
  node_list_interp_fixup(x, env, &rotation_info);
  return maybe_rotate(x, env, &rotation_info);
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


static void node_list_interp_fixup_rhs(sexp* parent, sexp* rhs, sexp* rhs_node,
                                       sexp* env, struct ast_rotation_info* info);

/**
 *
 * Takes a call to a binary operator whith problematic precedence
 * (between prec(`!`) and prec(`!!`)).
 *
 * - Somehow, root-swap parameter
 * - Normal interp on LHS
 * - Interp RHS with potential root swap
 * - If we get a root-swap, compare prec of pivot to prec of current
 *
 */
static sexp* node_list_interp_fixup(sexp* x, sexp* env, struct ast_rotation_info* info) {
  sexp* lhs_node = r_node_cdr(x);
  sexp* rhs_node = r_node_cddr(x);
  sexp* lhs = r_node_car(lhs_node);
  sexp* rhs = r_node_car(rhs_node);

  // Expand the LHS normally, it never needs changes in the AST
  struct expansion_info expansion_info = is_big_bang_op(lhs);
  if (expansion_info.op == OP_EXPAND_UQS) {
    sexp* node = big_bang(expansion_info.operand, env, lhs_node, rhs_node);
    rhs_node = r_node_cdr(node);
  } else {
    r_node_poke_car(lhs_node, call_interp(r_node_car(lhs_node), env));
  }

  node_list_interp_fixup_rhs(x, rhs, rhs_node, env, info);
  return x;
}

static void node_list_interp_fixup_rhs(sexp* parent, sexp* rhs, sexp* rhs_node,
                                       sexp* env, struct ast_rotation_info* info) {
  // An upper pivot is an operand of a !! call that is a binary
  // operation whose precedence is problematic (between prec(`!`) and
  // prec(`!!`))
  sexp* upper_pivot = find_upper_pivot(rhs, info);
  if (upper_pivot) {
    info->lower_root = parent;

    // Reattach the RHS to the upper pivot stripped of its !! call
    // in case there is no rotation around the lower root
    r_node_poke_car(rhs_node, upper_pivot);

    // There might be a lower pivot, so we need to find it. Also find
    // the target of unquoting (leftmost leaf whose predecence is
    // greater than prec(`!!`)) and unquote it.
    find_lower_pivot(upper_pivot, env, info);

    return;
  }

  // If `rhs` is an operator that might be involved in a rotation
  // recurse with the fixup version
  if (expr_maybe_needs_fixup(rhs)) {
    node_list_interp_fixup(rhs, env, info);

    // This might the upper root around which to rotate
    if (info->upper_pivot_op && op_has_precedence(r_which_operator(rhs), info->upper_pivot_op)) {
      info->upper_root = rhs;
      info->root_parent = parent;
    }

    return;
  }

  // RHS is not a binary operation that might need changes in the AST
  // so expand it as usual
  r_node_poke_car(rhs_node, call_interp(rhs, env));
}
