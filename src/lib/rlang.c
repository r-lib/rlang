#include <rlang.h>

void r_init_library_env();
void r_init_library_stack();
void r_init_library_sym();


// This *must* be called before making any calls to the functions
// provided in the library
void r_init_library() {
  r_init_library_sym();  // Needs to be first
  r_init_library_env();
  r_init_library_stack();

  r_quo_get_expr = (sexp* (*)(sexp*)) r_peek_c_callable("rlang", "rlang_quo_get_expr");
  r_quo_set_expr = (sexp* (*)(sexp*, sexp*)) r_peek_c_callable("rlang", "rlang_quo_set_expr");
  r_quo_get_env = (sexp* (*)(sexp*)) r_peek_c_callable("rlang", "rlang_quo_get_env");
  r_quo_set_env = (sexp* (*)(sexp*, sexp*)) r_peek_c_callable("rlang", "rlang_quo_set_env");

  /* parse.c - r_ops_precedence[] */
  RLANG_ASSERT((sizeof(r_ops_precedence) / sizeof(struct r_op_precedence)) == R_OP_MAX);

  for (int i = R_OP_NONE + 1; i < R_OP_MAX; ++i) {
    if (r_ops_precedence[i].power == 0) {
      r_abort("Internal error: `r_ops_precedence` is not fully initialised");
    }
  }
}
