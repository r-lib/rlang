#include <rlang.h>

#include "expr-interp.h"
#include "expr-interp-rotate.h"


/**
 * DOC: Interpolation in operator calls whose precedence might need fixup
 *
 * We want `!!` to have the precedence of unary `-` and `+` instead of
 * the very low precedence of `!`. To that end we need to patch the
 * AST to reflect the new precedence.
 *
 * Let's take `1 + 2 + 3` as a motivating example. `+` is a
 * left-associative operator so the expression `1 + 2` on the left is
 * evaluated first and it is pulled downwards in the AST:
 *
 *     > 1 + 2 + 3
 *
 *     █─`+`
 *     ├─█─`+`
 *     │ ├─1
 *     │ └─2
 *     └─3
 *
 * After introducing an unary operator with low precedence in the
 * expression we get this AST:
 *
 *     > 1 + !2 + 3
 *
 *     █─`+`
 *     ├─1
 *     └─█─`!`
 *       └─█─`+`
 *         ├─2
 *         └─3
 *
 * Every binary operation on the RHS of `!` that has a higher
 * precedence will be evaluated before `!`. As a result the second `+`
 * never gets the chance of being matched to the first one, it is cut
 * out of the LHS of `!`. The effect of `!` on the AST is equivalent
 * to wrapping the problematic expression in parentheses.
 *
 *     > 1 + (2 + 3)
 *
 *     █─`+`
 *     ├─1
 *     └─█─`(`
 *       └─█─`+`
 *         ├─2
 *         └─3
 *
 * This is only problematic when the precedence of the `!!` operand is
 * lower than the precedence of its parent operation. If it is higher,
 * the implicit grouping is the same as the one produced by `!`:
 *
 *     > ast(1 + 2 * 3)  // Implicit grouping
 *
 *     █─`+`
 *     ├─1
 *     └─█─`*
 *       ├─2
 *       └─3
 *
 *     > ast(1 + !2 * 3)  // `!` grouping
 *
 *     █─`+`
 *     ├─1
 *     └─█─`!`
 *       └─█─`*
 *         ├─2
 *         └─3
 *
 * If the precedence of `!`'s operand is lower the R parser will
 * unduly pull it downward in the AST. We can fix that by swapping the
 * operand with the parent node of `!`. In addition the LHS of the
 * operand (e.g. `2`) must become the RHS of the expression it was cut
 * off from. It turns out that these two operations amount to a [tree
 * rotation](https://en.wikipedia.org/wiki/Tree_rotation). The parent
 * node of `!` is the root (or rotator) and the `!` operand is the
 * pivot. We also need to take care of the actual expression that
 * needs to be unquoted, which we will call "target":
 *
 *     > 1 + !!2 + 3
 *
 *     █─`+`         // root
 *     ├─1
 *     └─█─`!`
 *       └─█─`!`
 *         └─█─`+`   // pivot
 *           ├─2     // target
 *           └─3
 *
 * Notice from the diagrams above that the leaves of the AST have the
 * same ordering no matter the operation precedence. When we patch up
 * the AST we only change the structure of the tree not the ordering
 * of the leaves. Tree rotation adequately preserves the ordering of
 * the leaves (which is why it useful for balancing ordered binary
 * trees).
 *
 * The rotation algorithm roughly goes as follows:
 *
 * - The `!!` target is unquoted and replaced with the unquoted value.
 * - The RHS of the root is attached to the LHS of the pivot.
 * - The LHS of the pivot is attached to the root.
 * - The root's parent is reattached to the pivot.
 *
 * The full story is a bit more complicated when complex expressions
 * are involved. There are three main complications. First the target
 * might not be a child of the pivot. Let's take this expression:
 *
 *     > 1 + 2 * 3 + 4
 *
 *     █─`+`
 *     ├─█─`+`
 *     │ ├─1
 *     │ └─█─`*`
 *     │   ├─2
 *     │   └─3
 *     └─4
 *
 * and assume we want to unquote `2`:
 *
 *     > 1 + !!2 * 3 + 4
 *
 *     █─`+`           // root
 *     ├─1
 *     └─█─`!`
 *       └─█─`!`
 *         └─█─`+`     // pivot
 *           ├─█─`*`
 *           │ ├─2     // target
 *           │ └─3
 *           └─4
 *
 * The `*` call is not a pivot because it has higher precedence than
 * the root `+`. Instead the pivot is the second `+` call. However
 * `!!` has higher precedence than `*` so the target to unquote is
 * deeper than the LHS of the pivot. In this case it is the LHS of the
 * LHS (it might be deeper but always across LHS's).
 *
 * Another source of complication is that we might need to rotate
 * entire subsets of the AST. First the pivot might comprise several
 * expressions. In this case we distinguish the lower pivot as the
 * node whose LHS is attached to the root and the upper pivot which
 * becomes the new root after rotation. This complication arises when
 * the `!!` operand is a succession of operations with decreasing
 * precedence (which is the case for left-associative operators with
 * the same precedence).
 *
 *     > 1 + 2 + 3 + 4 + 5
 *
 *     █─`+`
 *     ├─█─`+`
 *     │ ├─█─`+`
 *     │ │ ├─█─`+`
 *     │ │ │ ├─1
 *     │ │ │ └─2
 *     │ │ └─3
 *     │ └─4
 *     └─5
 *
 *     > 1 + !!2 + 3 + 4 + 5
 *
 *     █─`+`              // root
 *     ├─1
 *     └─█─`!`
 *       └─█─`!`
 *         └─█─`+`        // upper pivot
 *           ├─█─`+`
 *           │ ├─█─`+`    // lower pivot
 *           │ │ ├─2      // target
 *           │ │ └─3
 *           │ └─4
 *           └─5
 *
 * Finally the root might also comprise several expressions. In the
 * following example we see an upper root (which becomes the pivot's
 * or lower pivot's LHS) and a lower root (whose RHS is attached to
 * the pivot's or lower pivot's LHS). This complication happens when
 * the operations before `!!` have increasing levels of precedence:
 *
 *     > 1 + 2 * 3 + 4
 *
 *     █─`+`
 *     ├─█─`+`
 *     │ ├─1
 *     │ └─█─`*`
 *     │   ├─2
 *     │   └─3
 *     └─4
 *
 *     > 1 + 2 * !!3 + 4
 *
 *     █─`+`           // upper root
 *     ├─1
 *     └─█─`*`         // lower root
 *       ├─2
 *       └─█─`!`
 *         └─█─`!`
 *           └─█─`+`   // pivot
 *             ├─3     // target
 *             └─4
 *
 * These three complications (deep target, root, and pivot) may arise
 * in conjunction.
 *
 * In addition we also need to deal with multiple `!!` calls in a
 * series of binary operations. This is handled by recursing from the
 * upper pivot (the new root) after rotation. Finally the possibility
 * of intervening unary `+` or `-` operations also needs special
 * handling.
 *
 * All operators whose precedence lies between prec(`!`) and
 * prec(`!!`) might be involved in such a fixup of the AST. We call
 * these the "problematic" operators. Since the root can be multiple
 * expressions deep, we can't tell in advance whether the current
 * operation in the AST is involved in a rotation. Hence we apply
 * node_list_interp_fixup() instead of node_list_interp() whenever we
 * reach a problematic operator.
 */


bool op_is_unary(enum r_operator op) {
  if (op == R_OP_NONE || op > R_OP_MAX) {
    r_abort("Internal error: `enum r_operator` out of bounds");
  }
  return r_ops_precedence[op].unary;
}
bool is_unary(sexp* x) {
  return op_is_unary(r_which_operator(x));
}

bool op_is_unary_plusminus(enum r_operator op) {
  switch (op) {
  case R_OP_PLUS_UNARY:
  case R_OP_MINUS_UNARY:
    return true;
  default:
    return false;
  }
}
bool is_unary_plusminus(sexp* x) {
  return op_is_unary_plusminus(r_which_operator(x));
}

/**
 * struct ast_rotation_info - Rotation data gathered while recursing over AST
 *
 * @upper_pivot_op: The operation type of the upper pivot.
 * @upper_pivot: The expression that becomes the new root after rotation.
 * @lower_pivot: The expression whose LHS is attached to @upper_root.
 * @upper_root: The expression that becomes the LHS of @lower_pivot.
 * @lower_root: The expression whose RHS is attached to the LHS of @lower_pivot.
 * @root_parent: Node whose CAR should be reattached to @upper_pivot
 *   after rotation.
 */
struct ast_rotation_info {
  enum r_operator upper_pivot_op;
  sexp* upper_pivot;
  sexp* lower_pivot;
  sexp* upper_root;
  sexp* lower_root;
  sexp* root_parent;
};

static void initialise_rotation_info(struct ast_rotation_info* info) {
  info->upper_pivot_op = R_OP_NONE;
  info->upper_pivot = NULL;
  info->lower_pivot = NULL;
  info->upper_root = NULL;
  info->lower_root = NULL;
  info->root_parent = NULL;
}

// Defined below
static sexp* node_list_interp_fixup(sexp* x, sexp* parent, sexp* env,
                                    struct ast_rotation_info* rotation_info,
                                    bool expand_lhs);

/**
 * maybe_rotate() - Rotate if we found a pivot
 *
 * @op: Problematic operator.
 * @env: The unquoting environment.
 * @info: See &struct ast_rotation_info.
 *
 * If @op has precedence over the upper pivot, this is the upper
 * root. Otherwise use &ast_rotation_info->upper_root. If the latter
 * is not defined, this means no rotation is needed because the effect
 * of `!` on the AST corresponds to the implicit grouping (e.g. with
 * `1 + !!2 * 3`).
 */
static sexp* maybe_rotate(sexp* op, sexp* env, struct ast_rotation_info* info) {
  if (info->upper_pivot_op == R_OP_NONE) {
    return op;
  }

  // Rotate if `op` is the upper root
  if (r_lhs_op_has_precedence(r_which_operator(op), info->upper_pivot_op)) {
    // Swap the lower root's RHS with the lower pivot's LHS
    r_node_poke_car(info->lower_root, r_node_cadr(info->lower_pivot));
    r_node_poke_cadr(info->lower_pivot, op);

    // After rotation the upper pivot is the new root
    op = info->upper_pivot;
  } else if (info->upper_root) {
    r_node_poke_car(info->lower_root, r_node_cadr(info->lower_pivot));
    r_node_poke_cadr(info->lower_pivot, info->upper_root);
    r_node_poke_car(r_node_cddr(info->root_parent), info->upper_pivot);
  }
  // else there is no rotation needed

  // Reinitialise the `ast_rotation_info` on the stack in order to
  // reuse it in the recursion
  initialise_rotation_info(info);

  // Recurse on the RHS of the upper pivot (which is now the new root)
  node_list_interp_fixup(op, NULL, env, info, false);
  return maybe_rotate(op, env, info);
}

/**
 * fixup_interp() - Expand a problematic operation
 *
 * @x: A problematic operation, i.e. a call to an operator whose
 *   precedence is between that of `!` and that of `!!`.
 * @env: The unquoting environment.
 *
 * The expression to expand is an operator that might need changes in
 * the AST if we find a `!!` call down the line. From this point on
 * there is a &struct ast_rotation_info on the stack.
 */
sexp* fixup_interp(sexp* x, sexp* env) {
  // Happens with constructed calls without arguments such as `/`()
  if (r_node_cdr(x) == r_null) {
    return x;
  }

  struct ast_rotation_info rotation_info;
  initialise_rotation_info(&rotation_info);

  // Look for problematic !! calls and expand arguments on the way.
  // If a pivot is found rotate it around `x`.
  node_list_interp_fixup(x, NULL, env, &rotation_info, true);
  return maybe_rotate(x, env, &rotation_info);
}

/**
 * fixup_interp_first() - Expand a problematic operation starting with `!!`
 *
 * @x: A problematic operation whose LHS is a `!!` call, e.g. `!!1 + 2 + 3`.
 * @env: The unquoting environment.
 *
 * If `!!` is the root expression there is no rotation needed. Just
 * unquote the leftmost child across problematic binary operators.
 * However the resulting root might be involved in a rotation for a
 * subsequent `!!` call.
 */
sexp* fixup_interp_first(sexp* x, sexp* env) {
  sexp* parent = NULL; // `parent` will always be initialised in the loop
  sexp* target = x;
  while (is_problematic_op((parent = target, target = r_node_cadr(target)))
         && !is_unary(target));

  // Unquote target
  r_node_poke_cadr(parent, r_eval(target, env));

  // Expand the new root but no need to expand LHS as we just unquoted it
  struct ast_rotation_info rotation_info;
  initialise_rotation_info(&rotation_info);

  node_list_interp_fixup(x, NULL, env, &rotation_info, true);
  return maybe_rotate(x, env, &rotation_info);
}

/**
 * find_upper_pivot() - Find upper pivot
 *
 * @x: An expression.
 * @info: See &struct ast_rotation_info.
 *
 * Detect `!!` call structures. The operand is the upper pivot. Fill
 * in &ast_rotation_info->upper_pivot_op and
 * &ast_rotation_info->upper_pivot within @info.
 */
static void find_upper_pivot(sexp* x, struct ast_rotation_info* info) {
  if (!r_is_call(x, "!")) {
    return;
  }

  x = r_node_cadr(x);
  if (!r_is_call(x, "!")) {
    return;
  }

  x = r_node_cadr(x);
  if (r_is_call(x, "!")) {
    return;
  }

  enum r_operator op = r_which_operator(x);
  if (!op_needs_fixup(op)) {
    return;
  }

  info->upper_pivot_op = op;
  info->upper_pivot = x;
}

/**
 * find_lower_pivot() - Find lower pivot and unquote target
 *
 * @x: This is the upper pivot in the first call and the LHS of the
 *   previous node when recursing.
 * @parent_node: Used to handle unary `+` and `-`, e.g. `1 + !!-2 + 3`.
 * @env: Unquoting environment.
 * @info: See &struct ast_rotation_info.
 *
 * Climb through LHS's until we find an operator that has greater
 * precendence than the upper pivot. This node is the lower pivot
 * whose LHS will be attached to the upper root. Continue climbing the
 * LHS's until we find the target and unquote it in place. Expand all
 * RHS's on the way there.
 *
 * Fill in &ast_rotation_info->lower_pivot within @info.
 */
static void find_lower_pivot(sexp* x, sexp* parent_node, sexp* env,
                             struct ast_rotation_info* info) {
  sexp* lhs_node = r_node_cdr(x);
  sexp* rhs_node = r_node_cdr(lhs_node);

  // We found an unary `+` or `-` on the way
  if (rhs_node == r_null) {
    sexp* target = r_eval(x, env);

    if (parent_node) {
      r_node_poke_car(parent_node, target);
    } else {
      r_node_poke_car(info->lower_root, target);
      // If there is no parent x there is no operator precedence to
      // fix so abort recursion
      initialise_rotation_info(info);
    }
    return;
  }

  // Only expand RHS if not the upper pivot because there might be
  // consecutive rotations needed. The upper pivot's RHS will be
  // expanded after the current rotation is complete.
  if (x != info->upper_pivot) {
    r_node_poke_car(rhs_node, call_interp(r_node_car(rhs_node), env));
  }

  sexp* lhs = r_node_car(lhs_node);
  enum r_operator lhs_op = r_which_operator(lhs);
  if (!op_needs_fixup(lhs_op)) {
    if (!info->lower_pivot) {
      info->lower_pivot = x;
    }

    sexp* target = r_eval(lhs, env);
    r_node_poke_cadr(x, target);

    // Stop recursion as we found both target and lower pivot
    return;
  }

  if (!r_lhs_op_has_precedence(info->upper_pivot_op, lhs_op)) {
    info->lower_pivot = x;
  }

  // Recurse
  find_lower_pivot(lhs, lhs_node, env, info);
}


// Defined below
static void node_list_interp_fixup_rhs(sexp* rhs, sexp* rhs_node, sexp* parent,
                                       sexp* env, struct ast_rotation_info* info);

/**
 * node_list_interp_fixup() - Expansion for binary operators that might need fixup
 *
 * @x A call to a binary operator whith problematic precedence
 *   (between prec(`!`) and prec(`!!`)).
 * @env The environment where to unquote the `!!` target.
 * @parent Needed to handle a mix of unary and binary operators
 *   supplied to the unquote operator, e.g. `!!-1 + 2`. This is the
 *   outer call of which `x` is an argument, or the C `NULL` if there
 *   is none.
 * @info Information about the pivot, the root and the unquoted target.
 * @expand_lhs Whether to expand the LHS. In some cases (e.g. after a
 *   rotation) it is not necessary to expand the LHS as it was already
 *   visited.
 */
static sexp* node_list_interp_fixup(sexp* x, sexp* parent, sexp* env,
                                    struct ast_rotation_info* info,
                                    bool expand_lhs) {
  sexp* lhs_node = r_node_cdr(x);
  sexp* lhs = r_node_car(lhs_node);

  // If there's a unary `+` or `-` on the way recurse on its RHS
  if (is_unary_plusminus(x)) {
    node_list_interp_fixup_rhs(lhs, lhs_node, parent, env, info);
    return x;
  }


  sexp* rhs_node = r_node_cddr(x);
  sexp* rhs = r_node_car(rhs_node);

  if (expand_lhs) {
    // Expand the LHS normally, it never needs changes in the AST
    r_node_poke_car(lhs_node, call_interp(r_node_car(lhs_node), env));
  }

  node_list_interp_fixup_rhs(rhs, rhs_node, x, env, info);
  return x;
}

/**
 * node_list_interp_fixup_rhs() - Expansion for binary operators that might need fixup
 *
 * @rhs: The right-hand side argument of an operator with problematic
 *   precedence.
 * @rhs_node: Parent node of RHS. If `rhs` is a `!!` call, we reattach
 *   the `!!` operand to its parent node `rhs_node`.
 * @parent: See node_list_interp_fixup().
 * @env: The unquoting environment.
 * @info: See &struct ast_rotation_info.
 */
static void node_list_interp_fixup_rhs(sexp* rhs, sexp* rhs_node, sexp* parent,
                                       sexp* env, struct ast_rotation_info* info) {
  // Happens with constructed calls like `/`(1)
  if (rhs_node == r_null) {
    return;
  }

  // An upper pivot is an operand of a !! call that is a binary
  // operation whose precedence is problematic (between prec(`!`) and
  // prec(`!!`))
  find_upper_pivot(rhs, info);
  if (info->upper_pivot) {
    info->lower_root = rhs_node;

    // There might be a lower pivot, so we need to find it. Also find
    // the target of unquoting (leftmost leaf whose predecence is
    // greater than prec(`!!`)) and unquote it.
    find_lower_pivot(info->upper_pivot, NULL, env, info);

    if (info->upper_pivot) {
      // Reattach the RHS to the upper pivot stripped of its !! call
      // in case there is no rotation around the lower root
      r_node_poke_car(rhs_node, info->upper_pivot);
    }

    return;
  }

  // If `rhs` is an operator that might be involved in a rotation
  // recurse with the fixup version
  if (is_problematic_op(rhs)) {
    node_list_interp_fixup(rhs, parent, env, info, true);

    // This might the upper root around which to rotate
    if (info->upper_pivot_op
        && r_lhs_op_has_precedence(r_which_operator(rhs), info->upper_pivot_op)) {
      info->upper_root = rhs;
      info->root_parent = parent;
    }

    return;
  }

  // RHS is not a binary operation that might need changes in the AST
  // so expand it as usual
  r_node_poke_car(rhs_node, call_interp(rhs, env));
}
