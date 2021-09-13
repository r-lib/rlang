#include <rlang.h>
#include "nse-inject.h"
#include "ast-rotate.h"
#include "utils.h"


struct injection_info which_bang_op(r_obj* second, struct injection_info info);
struct injection_info which_curly_op(r_obj* second, struct injection_info info);

struct injection_info which_uq_op(r_obj* first) {
  struct injection_info info = init_expansion_info();

  if (r_is_call(first, "(")) {
    r_obj* paren = r_node_cadr(first);
    if (r_is_call(paren, "(")) {
      return info;
    }

    struct injection_info inner_info = which_uq_op(paren);

    // Check that `root` is NULL so we don't remove parentheses when
    // there's an operation tail (i.e. when the parse tree was fixed
    // up to bind tightly)
    if (inner_info.op == INJECTION_OP_uq && inner_info.root == r_null) {
      return inner_info;
    } else {
      return info;
    }
  }

  if (r_typeof(first) != R_TYPE_call) {
    return info;
  }

  r_obj* head = r_node_car(first);

  if (r_typeof(head) != R_TYPE_symbol) {
    return info;
  }

  const char* nm = r_sym_c_string(head);

  if (strcmp(nm, "!") == 0) {
    return which_bang_op(r_node_cadr(first), info);
  } else if (strcmp(nm, "{") == 0) {
    return which_curly_op(first, info);
  } else {
    return info;
  }
}

struct injection_info which_bang_op(r_obj* second, struct injection_info info) {
  if (!r_is_call(second, "!")) {
    return info;
  }

  r_obj* third = r_node_cadr(second);

  // Need to fill in `info` for `!!` because parse tree might need changes
  if (!r_is_call(third, "!")) {
    if (is_problematic_op(third)) {
      info.op = INJECTION_OP_fixup;
      info.operand = third;
    } else {
      info.op = INJECTION_OP_uq;
      info.parent = r_node_cdr(second);
      info.operand = third;
    }
    return info;
  }

  info.op = INJECTION_OP_uqs;
  info.operand = r_node_cadr(third);
  return info;
}
struct injection_info which_curly_op(r_obj* first, struct injection_info info) {
  r_obj* first_cdr = r_node_cdr(first);
  r_obj* second = r_node_car(first_cdr);

  if (!r_is_call(second, "{") || r_node_cdr(first_cdr) != r_null) {
    return info;
  }

  info.op = INJECTION_OP_curly;
  info.parent = r_node_cdr(second);
  info.operand = r_node_cadr(second);

  return info;
}


// These functions are questioning and might be soft-deprecated in the
// future
void signal_uq_soft_deprecation() {
  return ;
  const char* msg =
    "`UQ()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!` instead.";
  signal_soft_deprecated(msg, msg, r_envs.empty);
}
void signal_uqs_soft_deprecation() {
  return ;
  const char* msg =
    "`UQS()` is soft-deprecated as of rlang 0.2.0. "
    "Please use the prefix form of `!!!` instead.";
  signal_soft_deprecated(msg, msg, r_envs.empty);
}

void signal_namespaced_uq_deprecation() {
  warn_deprecated("namespaced rlang::UQ()",
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
  warn_deprecated("namespaced rlang::UQS()",
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

void maybe_poke_big_bang_op(r_obj* x, struct injection_info* info) {
  if (r_is_call(x, "!!!")) {
    if (r_node_cddr(x) != r_null) {
      r_abort("Can't supply multiple arguments to `!!!`");
    }
    info->op = INJECTION_OP_uqs;
    info->operand = r_node_cadr(x);
    return ;
  }

  // Handle expressions like foo::`!!`(bar) or foo$`!!`(bar)
  if (r_is_prefixed_call(x, "!!!")) {
    const char* name = r_sym_c_string(r_node_caar(x));
    r_abort("Prefix form of `!!!` can't be used with `%s`", name);
  }

  bool namespaced_uqs = r_is_namespaced_call(x, "rlang", "UQS");
  if (namespaced_uqs) {
    signal_namespaced_uqs_deprecation();
  }
  if (namespaced_uqs || r_is_call(x, "UQS")) {
    signal_uqs_soft_deprecation();
    info->op = INJECTION_OP_uqs;
    info->operand = r_node_cadr(x);
    return ;
  }
}

static r_obj* dot_data_sym = NULL;

struct injection_info which_expansion_op(r_obj* x, bool unquote_names) {
  struct injection_info info = which_uq_op(x);

  if (r_typeof(x) != R_TYPE_call) {
    return info;
  }
  if (info.op) {
    return info;
  }

  if (is_problematic_op(x)) {
    info.op = INJECTION_OP_fixup;
    return info;
  }

  if (unquote_names && r_is_call(x, ":=")) {
    info.op = INJECTION_OP_uqn;
    return info;
  }


  if (r_is_call(x, "!!")) {
    info.op = INJECTION_OP_uq;
    info.operand = r_node_cadr(x);
    return info;
  }

  // Handle expressions like foo::`!!`(bar) or foo$`!!`(bar)
  if (r_is_prefixed_call(x, "!!")) {
    info.op = INJECTION_OP_uq;
    info.operand = r_node_cadr(x);
    info.parent = r_node_cdr(r_node_cdar(x));
    info.root = r_node_car(x);
    return info;
  }

  maybe_poke_big_bang_op(x, &info);
  if (info.op == INJECTION_OP_uqs) {
    return info;
  }

  // This logic is complicated because rlang::UQ() gets fully unquoted
  // but not foobar::UQ(). The functional form UI is now retired so
  // we'll be able to simplify this in the future.
  if (r_is_prefixed_call(x, "UQ")) {
    signal_uq_soft_deprecation();

    info.op = INJECTION_OP_uq;
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
    info.op = INJECTION_OP_uq;
    info.operand = r_node_cadr(x);
    return info;
  }

  if (r_is_call(x, "[[") && r_node_cadr(x) == dot_data_sym) {
    info.op = INJECTION_OP_dot_data;
    info.root = x;
    info.parent = r_node_cddr(x);
    info.operand = r_node_car(info.parent);

    // User had to unquote operand manually before .data[[ was unquote syntax
    struct injection_info nested = which_expansion_op(info.operand, false);
    if (nested.op == INJECTION_OP_uq) {
      const char* msg = "It is no longer necessary to unquote within the `.data` pronoun";
      signal_soft_deprecated(msg, msg, r_envs.empty);
      info.operand = nested.operand;
    }

    return info;
  }

  return info;
}

struct injection_info is_big_bang_op(r_obj* x) {
  struct injection_info info = which_uq_op(x);

  if (info.op != INJECTION_OP_uqs) {
    maybe_poke_big_bang_op(x, &info);
  }

  return info;
}


static r_obj* bang_bang_teardown(r_obj* value, struct injection_info info) {
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
static r_obj* bang_bang(struct injection_info info, r_obj* env) {
  r_obj* value = r_eval(info.operand, env);
  return bang_bang_teardown(value, info);
}

// From dots.c
r_obj* big_bang_coerce_pairlist(r_obj* x, bool deep);

r_obj* big_bang(r_obj* operand, r_obj* env, r_obj* prev, r_obj* node) {
  r_obj* value = KEEP(r_eval(operand, env));
  value = big_bang_coerce_pairlist(value, true);

  if (value == r_null) {
    // Remove `!!!foo` from pairlist of args
    r_node_poke_cdr(prev, r_node_cdr(node));
    node = prev;
  } else {
    // Insert coerced value into existing pairlist of args
    r_obj* tail = r_pairlist_tail(value);
    r_node_poke_cdr(tail, r_node_cdr(node));
    r_node_poke_cdr(prev, value);
    node = tail;
  }

  FREE(1);
  return node;
}

static r_obj* curly_curly(struct injection_info info, r_obj* env) {
  r_obj* value = ffi_enquo(info.operand, env);
  return bang_bang_teardown(value, info);
}


// Defined below
static r_obj* call_list_interp(r_obj* x, r_obj* env);
static r_obj* node_list_interp(r_obj* x, r_obj* env);
static void call_maybe_poke_string_head(r_obj* call);

r_obj* call_interp(r_obj* x, r_obj* env)  {
  struct injection_info info = which_expansion_op(x, false);
  return call_interp_impl(x, env, info);
}

r_obj* call_interp_impl(r_obj* x, r_obj* env, struct injection_info info) {
  if (info.op && info.op != INJECTION_OP_fixup && r_node_cdr(x) == r_null) {
    r_abort("`UQ()` and `UQS()` must be called with an argument");
  }

  switch (info.op) {
  case INJECTION_OP_none:
    if (r_typeof(x) != R_TYPE_call) {
      return x;
    } else {
      r_obj* out = call_list_interp(x, env);
      call_maybe_poke_string_head(out);
      return out;
    }
  case INJECTION_OP_uq:
    return bang_bang(info, env);
  case INJECTION_OP_curly:
    return curly_curly(info, env);
  case INJECTION_OP_dot_data: {
    r_obj* out = KEEP(bang_bang(info, env));

    // Replace symbols by strings
    r_obj* subscript_node = r_node_cddr(out);
    r_obj* subscript = r_node_car(subscript_node);

    if (is_quosure(subscript)) {
      subscript = r_node_cadr(subscript);
    }
    if (r_typeof(subscript) == R_TYPE_symbol) {
      subscript = r_sym_as_utf8_character(subscript);
      r_node_poke_car(subscript_node, subscript);
    }

    FREE(1);
    return out;
  }
  case INJECTION_OP_fixup:
    if (info.operand == r_null) {
      return fixup_interp(x, env);
    } else {
      return fixup_interp_first(info.operand, env);
    }
  case INJECTION_OP_uqs:
    r_abort("Can't use `!!!` at top level.");
  case INJECTION_OP_uqn:
    r_abort("Internal error: Deep `:=` unquoting.");
  }

  // Silence noreturn warning on GCC
  r_abort("Never reached.");
}

// Make (!!"foo")() and "foo"() equivalent
static void call_maybe_poke_string_head(r_obj* call) {
  r_obj* head = r_node_car(call);
  if (r_typeof(head) != R_TYPE_character) {
    return ;
  }

  r_ssize n = r_length(head);
  if (n != 1) {
    r_abort("Unquoted function name must be a character vector of length 1");
  }
  r_node_poke_car(call, r_sym(r_chr_get_c_string(head, 0)));
}

static r_obj* call_list_interp(r_obj* x, r_obj* env) {
  r_node_poke_car(x, call_interp(r_node_car(x), env));
  r_node_poke_cdr(x, node_list_interp(r_node_cdr(x), env));
  return x;
}
static r_obj* node_list_interp(r_obj* node, r_obj* env) {
  r_obj* prev = KEEP(r_new_node(r_null, node));
  r_obj* out = prev;

  while (node != r_null) {
    r_obj* arg = r_node_car(node);
    struct injection_info info = which_expansion_op(arg, false);

    if (info.op == INJECTION_OP_uqs) {
      node = big_bang(info.operand, env, prev, node);
    } else {
      r_node_poke_car(node, call_interp_impl(arg, env, info));
    }

    prev = node;
    node = r_node_cdr(node);
  }

  FREE(1);
  return r_node_cdr(out);
}

r_obj* ffi_interp(r_obj* x, r_obj* env) {
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }
  if (r_typeof(x) != R_TYPE_call) {
    return x;
  }

  // FIXME: Only duplicate the call tree, not the leaves
  x = KEEP(r_copy(x));
  x = call_interp(x, env);

  FREE(1);
  return x;
}


void rlang_init_expr_interp() {
  dot_data_sym = r_sym(".data");
}
