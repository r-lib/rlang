#ifndef RLANG_INTERNAL_NSE_INJECT_H
#define RLANG_INTERNAL_NSE_INJECT_H

#include <rlang.h>

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


enum injection_op {
  INJECTION_OP_none,
  INJECTION_OP_uq,
  INJECTION_OP_uqs,
  INJECTION_OP_uqn,
  INJECTION_OP_fixup,
  INJECTION_OP_dot_data,
  INJECTION_OP_curly
};
#define INJECTION_OP_MAX 7

struct injection_info {
  enum injection_op op;
  r_obj* operand;  // Expression being unquoted
  r_obj* parent;   // Node pointing to the future unquoted value
  r_obj* root;     // Expression wrapping the unquoted value (optional)
};

static inline struct injection_info init_expansion_info(void) {
  struct injection_info info;

  info.op = INJECTION_OP_none;
  info.operand = r_null;
  info.parent = r_null;
  info.root = r_null;

  return info;
}

struct injection_info which_uq_op(r_obj* x);
struct injection_info which_expansion_op(r_obj* x, bool unquote_names);
struct injection_info is_big_bang_op(r_obj* x);

r_obj* big_bang_coerce(r_obj* expr);

r_obj* ffi_interp(r_obj* x, r_obj* env);
r_obj* call_interp(r_obj* x, r_obj* env);
r_obj* call_interp_impl(r_obj* x, r_obj* env, struct injection_info info);


static inline r_obj* forward_quosure(r_obj* x, r_obj* env) {
  switch (r_typeof(x)) {
  case R_TYPE_call:
    if (is_quosure(x)) {
      return x;
    }
    // else fallthrough
  case R_TYPE_symbol:
  case R_TYPE_closure:
    return ffi_new_quosure(x, env);
  default:
    return ffi_new_quosure(x, r_envs.empty);
  }
}


#endif
