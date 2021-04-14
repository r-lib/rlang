#include <rlang.h>

#include "attrib.c"
#include "call.c"
#include "cnd.c"
#include "c-utils.c"
#include "debug.c"
#include "dict.c"
#include "df.c"
#include "dyn-array.c"
#include "dyn-list-of.c"
#include "env.c"
#include "env-binding.c"
#include "eval.c"
#include "export.c"
#include "fn.c"
#include "formula.c"
#include "globals.c"
#include "node.c"
#include "obj.c"
#include "parse.c"
#include "quo.c"
#include "session.c"
#include "stack.c"
#include "sym.c"
#include "vec.c"
#include "vec-chr.c"
#include "vec-lgl.c"
#include "vendor.c"
#include "walk.c"


// Allows long vectors to be indexed with doubles
r_ssize r_as_ssize(r_obj* n) {
  switch (r_typeof(n)) {

  case R_TYPE_double: {
    if (r_length(n) != 1) {
      goto invalid;
    }
    double out = r_dbl_get(n, 0);
    if (out > R_SSIZE_MAX) {
      r_abort("`n` is too large a number");
    }
    return (r_ssize) floor(out);
  }

  case R_TYPE_integer: {
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
void r_init_library_call();
void r_init_library_cnd();
void r_init_library_dyn_array();
void r_init_library_env();
void r_init_library_fn();
void r_init_library_globals();
void r_init_library_globals_syms();
void r_init_library_session();
void r_init_library_obj(r_obj*);
void r_init_library_stack();
void r_init_library_vendor();

static r_obj* shared_x_env;
static r_obj* shared_xy_env;
static r_obj* shared_xyz_env;

// This *must* be called before making any calls to the functions
// provided in the library. Register this function in your init file
// and `.Call()` it from your `.onLoad()` hook.
r_obj* r_init_library(r_obj* ns) {
  if (!R_IsNamespaceEnv(ns)) {
    Rf_errorcall(r_null,
                 "Can't initialise rlang library.\n"
                 "x `ns` must be a namespace environment.");
  }

  // Need to be first
  r_init_library_vendor(); // Needed for xxh used in `r_preserve()`
  r_init_library_globals_syms();
  r_init_library_obj(ns);
  r_init_library_globals();

  r_init_rlang_ns_env();
  r_init_library_call();
  r_init_library_cnd();
  r_init_library_dyn_array();
  r_init_library_env();
  r_init_library_fn();
  r_init_library_session();
  r_init_library_stack();

  shared_x_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_base_env);
  r_preserve(shared_x_env);

  shared_xy_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_base_env);
  r_preserve(shared_xy_env);

  shared_xyz_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_base_env);
  r_preserve(shared_xyz_env);

  r_quo_get_expr = (r_obj* (*)(r_obj*)) r_peek_c_callable("rlang", "rlang_quo_get_expr");
  r_quo_set_expr = (r_obj* (*)(r_obj*, r_obj*)) r_peek_c_callable("rlang", "rlang_quo_set_expr");
  r_quo_get_env = (r_obj* (*)(r_obj*)) r_peek_c_callable("rlang", "rlang_quo_get_env");
  r_quo_set_env = (r_obj* (*)(r_obj*, r_obj*)) r_peek_c_callable("rlang", "rlang_quo_set_env");

  // Return a SEXP so the init function can be called from R
  return r_null;
}
