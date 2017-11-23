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


static inline bool is_rlang_call(SEXP x, const char* name) {
  return
    r_is_call(x, name) ||
    r_is_namespaced_call(x, "rlang", name);
}
static inline bool is_rlang_call_any(SEXP x, const char** names, int n) {
  return
    r_is_call_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}
static inline bool is_splice_call(SEXP node) {
  return is_rlang_call_any(node, uqs_names, UQS_N);
}


enum expansion_op {
  OP_EXPAND_NONE,
  OP_EXPAND_UQ,
  OP_EXPAND_UQE,
  OP_EXPAND_UQS,
};

struct expansion_info {
  enum expansion_op op;
  SEXP operand;
  SEXP parent;
  SEXP root;
};

static inline struct expansion_info init_expansion_info() {
  struct expansion_info info;

  info.op = OP_EXPAND_NONE;
  info.operand = r_null;
  info.parent = r_null;
  info.root = r_null;

  return info;
}

struct expansion_info which_bang_op(SEXP x);
struct expansion_info which_expansion_op(SEXP x);

SEXP big_bang_coerce(SEXP expr);

SEXP rlang_interp(SEXP x, SEXP env);
SEXP call_interp(SEXP x, SEXP env);
SEXP call_interp_impl(SEXP x, SEXP env, struct expansion_info info);


static inline SEXP rlang_forward_quosure(SEXP x, SEXP env) {
  if (r_is_quosure(x)) {
    return x;
  } else if (r_is_symbolic(x)) {
    return r_new_quosure(x, env);
  } else {
    return r_new_quosure(x, r_empty_env);
  }
}


#endif
