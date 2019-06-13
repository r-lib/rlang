#ifndef RLANG_INTERNAL_EXPR_INTERP_H
#define RLANG_INTERNAL_EXPR_INTERP_H

#include "quo.h"


#define UQ_N 2
#define UQS_N 2

static const char* uqs_names[UQS_N] = { "UQS", "!!!"};


static inline bool is_maybe_rlang_call(sexp* x, const char* name) {
  return
    r_is_call(x, name) ||
    r_is_namespaced_call(x, "rlang", name);
}
static inline bool is_maybe_rlang_call_any(sexp* x, const char** names, int n) {
  return
    r_is_call_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}
static inline bool is_splice_call(sexp* node) {
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
  sexp* operand;  // Expression being unquoted
  sexp* parent;   // Node pointing to the future unquoted value
  sexp* root;     // Expression wrapping the unquoted value (optional)
};

static inline struct expansion_info init_expansion_info() {
  struct expansion_info info;

  info.op = OP_EXPAND_NONE;
  info.operand = r_null;
  info.parent = r_null;
  info.root = r_null;

  return info;
}

struct expansion_info which_uq_op(sexp* x);
struct expansion_info which_expansion_op(sexp* x, bool unquote_names);
struct expansion_info is_big_bang_op(sexp* x);

sexp* big_bang_coerce(sexp* expr);

sexp* rlang_interp(sexp* x, sexp* env);
sexp* call_interp(sexp* x, sexp* env);
sexp* call_interp_impl(sexp* x, sexp* env, struct expansion_info info);


static inline sexp* forward_quosure(sexp* x, sexp* env) {
  switch (r_typeof(x)) {
  case r_type_call:
    if (rlang_is_quosure(x)) {
      return x;
    }
    // else fallthrough
  case r_type_symbol:
  case r_type_closure:
    return rlang_new_quosure(x, env);
  default:
    return rlang_new_quosure(x, r_empty_env);
  }
}


#endif
