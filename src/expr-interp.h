
#define UQ_N 3
#define UQS_N 2

static const char* uq_names[UQ_N] = { "UQ", "UQE", "!!" };
static const char* uqs_names[UQS_N] = { "UQS", "!!!"};


#define FIXUP_OPS_N 10
#define FIXUP_UNARY_OPS_N 2

static const char* fixup_ops_names[FIXUP_OPS_N] = {
  "<", ">", "<=", ">=", "==", "!=", "*", "/", ":", "^"
};
static const char* fixup_unary_ops_names[FIXUP_UNARY_OPS_N] = {
  "-", "+"
};


static inline bool is_rlang_call_any(SEXP x, const char** names, int n) {
  return
    r_is_call_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}
static inline bool is_splice_call(SEXP node) {
  return is_rlang_call_any(node, uqs_names, UQS_N);
}


#define OP_EXPAND_NONE 0
#define OP_EXPAND_UQ 1
#define OP_EXPAND_UQS 2

int bang_level(SEXP x, SEXP* operand);
int which_expand_op(SEXP x, SEXP* operand);


SEXP rlang_interp(SEXP x, SEXP env);
SEXP interp_lang(SEXP x, SEXP env);


static inline SEXP rlang_forward_quosure(SEXP x, SEXP env) {
  if (r_is_quosure(x)) {
    return x;
  } else if (r_is_symbolic(x)) {
    return r_new_quosure(x, env);
  } else {
    return r_new_quosure(x, r_empty_env);
  }
}
