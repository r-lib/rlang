#ifndef RLANG_INTERNAL_EXPR_INTERP_H
#define RLANG_INTERNAL_EXPR_INTERP_H


#define UQE_N 2
#define UQS_N 2

static const char* uqe_names[UQE_N] = { "UQE", "!!" };
static const char* uqs_names[UQS_N] = { "UQS", "!!!"};


#define FIXUP_OPS_N 10
#define FIXUP_UNARY_OPS_N 2

static const char* fixup_ops_names[FIXUP_OPS_N] = {
  "<", ">", "<=", ">=", "==", "!=", "*", "/", ":", "^"
};
static const char* fixup_unary_ops_names[FIXUP_UNARY_OPS_N] = {
  "-", "+"
};


static inline bool is_rlang_call(sexp* x, const char* name) {
  return
    r_is_call(x, name) ||
    r_is_namespaced_call(x, "rlang", name);
}
static inline bool is_rlang_call_any(sexp* x, const char** names, int n) {
  return
    r_is_call_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}
static inline bool is_splice_call(sexp* node) {
  return is_rlang_call_any(node, uqs_names, UQS_N);
}


#define N_EXPANSION_OPS 5

enum expansion_op {
  OP_EXPAND_NONE,
  OP_EXPAND_UQ,
  OP_EXPAND_UQE,
  OP_EXPAND_UQS,
  OP_EXPAND_UQN
};

struct expansion_info {
  enum expansion_op op;
  sexp* operand;
  sexp* parent;
  sexp* root;
};

static inline struct expansion_info init_expansion_info() {
  struct expansion_info info;

  info.op = OP_EXPAND_NONE;
  info.operand = r_null;
  info.parent = r_null;
  info.root = r_null;

  return info;
}

struct expansion_info which_bang_op(sexp* x);
struct expansion_info which_expansion_op(sexp* x, bool unquote_names);

sexp* big_bang_coerce(sexp* expr);

sexp* rlang_interp(sexp* x, sexp* env);
sexp* call_interp(sexp* x, sexp* env);
sexp* call_interp_impl(sexp* x, sexp* env, struct expansion_info info);


static inline sexp* forward_quosure(sexp* x, sexp* env) {
  if (r_is_quosure(x)) {
    return x;
  } else if (r_is_symbolic(x)) {
    return r_new_quosure(x, env);
  } else {
    return r_new_quosure(x, r_empty_env);
  }
}


#endif
