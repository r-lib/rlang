#include <rlang.h>


// Allows long vectors to be indexed with doubles
r_ssize r_as_ssize(sexp* n) {
  switch (r_typeof(n)) {

  case r_type_double: {
    if (r_length(n) != 1) {
      goto invalid;
    }
    double out = r_dbl_get(n, 0);
    if (out > R_SSIZE_MAX) {
      r_abort("`n` is too large a number");
    }
    return (r_ssize) floor(out);
  }

  case r_type_integer: {
    if (r_length(n) != 1) {
      goto invalid;
    }
    return (r_ssize) r_int_get(n, 0);
  }

  invalid:
  default:
    r_abort("Expected a scalar integer or double");
  }
}

void r_init_rlang_ns_env();
void r_init_library_arg();
void r_init_library_cnd();
void r_init_library_env();
void r_init_library_session();
void r_init_library_squash();
void r_init_library_stack();
void r_init_library_sym();
void r_init_library_vec();
void r_init_library_vec_chr();

sexp* r_shared_true;
sexp* r_shared_false;

static sexp* shared_x_env;
static sexp* shared_xy_env;
static sexp* shared_xyz_env;

// This *must* be called before making any calls to the functions
// provided in the library. Register this function in your init file
// and `.Call()` it from your `.onLoad()` hook.
SEXP r_init_library() {
  r_init_library_sym();  // Needs to be first

  r_init_rlang_ns_env();
  r_init_library_arg();
  r_init_library_cnd();
  r_init_library_env();
  r_init_library_session();
  r_init_library_squash();
  r_init_library_stack();
  r_init_library_vec();
  r_init_library_vec_chr();

  r_shared_true = r_new_vector(r_type_logical, 1);
  r_mark_precious(r_shared_true);
  r_mark_shared(r_shared_true);
  *r_lgl_deref(r_shared_true) = 1;

  r_shared_false = r_new_vector(r_type_logical, 1);
  r_mark_precious(r_shared_false);
  r_mark_shared(r_shared_false);
  *r_lgl_deref(r_shared_false) = 0;

  shared_x_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_base_env);
  r_mark_precious(shared_x_env);

  shared_xy_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_base_env);
  r_mark_precious(shared_xy_env);

  shared_xyz_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_base_env);
  r_mark_precious(shared_xyz_env);

  r_quo_get_expr = (sexp* (*)(sexp*)) r_peek_c_callable("rlang", "rlang_quo_get_expr");
  r_quo_set_expr = (sexp* (*)(sexp*, sexp*)) r_peek_c_callable("rlang", "rlang_quo_set_expr");
  r_quo_get_env = (sexp* (*)(sexp*)) r_peek_c_callable("rlang", "rlang_quo_get_env");
  r_quo_set_env = (sexp* (*)(sexp*, sexp*)) r_peek_c_callable("rlang", "rlang_quo_set_env");

  /* parse.c - r_ops_precedence[] */
  RLANG_ASSERT((sizeof(r_ops_precedence) / sizeof(struct r_op_precedence)) == R_OP_MAX);

  for (int i = R_OP_NONE + 1; i < R_OP_MAX; ++i) {
    if (r_ops_precedence[i].power == 0) {
      Rf_error("Internal error: `r_ops_precedence` is not fully initialised");
    }
  }

  // Return a SEXP so the init function can be called from R
  return r_null;
}
