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
  signal_soft_deprecated(
    "`UQ()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!` instead."
  );
}
void signal_uqs_soft_deprecation() {
  return ;
  signal_soft_deprecated(
    "`UQS()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!!` instead."
  );
}

void signal_namespaced_uq_deprecation() {
  r_warn_deprecated("namespaced rlang::UQ()",
    "Prefixing `UQ()` with the rlang namespace is deprecated as of rlang 0.3.0.\n"
    "Please use the non-prefixed form or `!!` instead.\n"
    "\n"
    "  # Bad:\n"
    "  rlang::expr(mean(rlang::UQ(var) * 100))\n"
    "\n"
    "  # Ok:\n"
    "  rlang::expr(mean(UQ(var) * 100))\n"
    "\n"
    "  # Good:\n"
    "  rlang::expr(mean(!!var * 100))\n"
  );
}
void signal_namespaced_uqs_deprecation() {
  r_warn_deprecated("namespaced rlang::UQS()",
    "Prefixing `UQS()` with the rlang namespace is deprecated as of rlang 0.3.0.\n"
    "Please use the non-prefixed form or `!!!` instead.\n"
    "\n"
    "  # Bad:\n"
    "  rlang::expr(mean(rlang::UQS(args)))\n"
    "\n"
    "  # Ok:\n"
    "  rlang::expr(mean(UQS(args)))\n"
    "\n"
    "  # Good:\n"
    "  rlang::expr(mean(!!!args))\n"
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
    const char* name = r_sym_get_c_string(r_node_caar(x));
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

static sexp* dot_data_sym = NULL;

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

  if (r_is_call(x, "[[") && r_node_cadr(x) == dot_data_sym) {
    info.op = OP_EXPAND_DOT_DATA;
    info.root = x;
    info.parent = r_node_cddr(x);
    info.operand = r_node_car(info.parent);

    // User had to unquote operand manually before .data[[ was unquote syntax
    struct expansion_info nested = which_expansion_op(info.operand, false);
    if (nested.op == OP_EXPAND_UQ) {
      const char* msg = "It is no longer necessary to unquote within the `.data` pronoun";
      r_signal_soft_deprecated(msg, msg, r_empty_env);
      info.operand = nested.operand;
    }

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
  r_mark_shared(value);

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

// From dots.c
void signal_retired_splice(sexp* env);

// Maintain parity with dots_big_bang_coerce() in dots.c.
// The `env` argument is only needed for the soft-deprecation warning.
static sexp* deep_big_bang_coerce(sexp* x, sexp* env) {
  switch (r_typeof(x)) {
  case r_type_null:
    return x;
  case r_type_pairlist:
    return r_duplicate(x, true);
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
  case r_type_list: {
    int n_protect = 0;
    if (r_is_object(x)) {
      x = KEEP_N(r_eval_with_x(as_list_call, r_base_env, x), n_protect);
    }
    x = r_vec_coerce(x, r_type_pairlist);
    FREE(n_protect);
    return x;
  }
  case r_type_s4: {
    x = KEEP(r_eval_with_x(as_list_s4_call, r_methods_ns_env, x));
    x = r_vec_coerce(x, r_type_pairlist);
    FREE(1);
    return x;
  }
  case r_type_call:
    if (r_is_symbol(r_node_car(x), "{")) {
      return r_node_cdr(x);
    }
    // else fallthrough
  case r_type_symbol: {
    signal_retired_splice(env);
    return r_new_node(x, r_null);
  }
  default:
    r_abort(
      "Can't splice an object of type `%s` because it is not a vector",
      r_type_as_c_string(r_typeof(x))
    );
  }
}

sexp* big_bang(sexp* operand, sexp* env, sexp* node, sexp* next) {
  sexp* value = KEEP(r_eval(operand, env));
  value = deep_big_bang_coerce(value, env);

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
static void call_maybe_poke_string_head(sexp* call);

sexp* call_interp(sexp* x, sexp* env)  {
  struct expansion_info info = which_expansion_op(x, false);
  return call_interp_impl(x, env, info);
}

sexp* call_interp_impl(sexp* x, sexp* env, struct expansion_info info) {
  if (info.op && info.op != OP_EXPAND_FIXUP && r_node_cdr(x) == r_null) {
    r_abort("`UQ()` and `UQS()` must be called with an argument");
  }
  if (info.op == OP_EXPAND_UQE) {
    r_stop_defunct("`UQE()` is defunct. Please use `!!get_expr(x)`");
  }

  switch (info.op) {
  case OP_EXPAND_NONE:
    if (r_typeof(x) != r_type_call) {
      return x;
    } else {
      sexp* out = node_list_interp(x, env);
      call_maybe_poke_string_head(out);
      return out;
    }
  case OP_EXPAND_UQ:
  case OP_EXPAND_DOT_DATA:
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

// Make (!!"foo")() and "foo"() equivalent
static void call_maybe_poke_string_head(sexp* call) {
  sexp* head = r_node_car(call);
  if (r_typeof(head) != r_type_character) {
    return ;
  }

  r_ssize n = r_length(head);
  if (n != 1) {
    r_abort("Unquoted function name must be a character vector of length 1");
  }
  r_node_poke_car(call, r_sym(r_chr_get_c_string(head, 0)));
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


void rlang_init_expr_interp() {
  dot_data_sym = r_sym(".data");
}
