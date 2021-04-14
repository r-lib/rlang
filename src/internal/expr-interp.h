#ifndef RLANG_INTERNAL_EXPR_INTERP_H
#define RLANG_INTERNAL_EXPR_INTERP_H

#include "quo.h"
#include "utils.h"


#define UQ_N 2
#define UQS_N 2

static const char* uqs_names[UQS_N] = { "UQS", "!!!"};


static inline bool is_maybe_rlang_call(r_obj* x, const char* name) {
  return
    r_is_call(x, name) ||
    r_is_namespaced_call(x, "rlang", name);
}
static inline bool is_maybe_rlang_call_any(r_obj* x, const char** names, int n) {
  return
    r_is_call_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}
static inline bool is_splice_call(r_obj* node) {
  return is_maybe_rlang_call_any(node, uqs_names, UQS_N);
}


#define EXPANSION_OP_MAX 7
enum expansion_op {
  OP_EXPAND_NONE,
  OP_EXPAND_UQ,
  OP_EXPAND_UQS,
  OP_EXPAND_UQN,
  OP_EXPAND_FIXUP,
  OP_EXPAND_DOT_DATA,
  OP_EXPAND_CURLY
};

struct expansion_info {
  enum expansion_op op;
  r_obj* operand;  // Expression being unquoted
  r_obj* parent;   // Node pointing to the future unquoted value
  r_obj* root;     // Expression wrapping the unquoted value (optional)
};

static inline struct expansion_info init_expansion_info() {
  struct expansion_info info;

  info.op = OP_EXPAND_NONE;
  info.operand = r_null;
  info.parent = r_null;
  info.root = r_null;

  return info;
}

struct expansion_info which_uq_op(r_obj* x);
struct expansion_info which_expansion_op(r_obj* x, bool unquote_names);
struct expansion_info is_big_bang_op(r_obj* x);

r_obj* big_bang_coerce(r_obj* expr);

r_obj* rlang_interp(r_obj* x, r_obj* env);
r_obj* call_interp(r_obj* x, r_obj* env);
r_obj* call_interp_impl(r_obj* x, r_obj* env, struct expansion_info info);


static inline r_obj* forward_quosure(r_obj* x, r_obj* env) {
  switch (r_typeof(x)) {
  case R_TYPE_call:
    if (rlang_is_quosure(x)) {
      return x;
    }
    // else fallthrough
  case R_TYPE_symbol:
  case R_TYPE_closure:
    return rlang_new_quosure(x, env);
  default:
    return rlang_new_quosure(x, r_empty_env);
  }
}


#endif
