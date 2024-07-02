#include <rlang.h>
#include <stdlib.h>

#include "arg.c"
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

#ifdef RLANG_USE_PRIVATE_ACCESSORS
  #include "walk.c"
#endif

// Allows long vectors to be indexed with doubles
r_ssize r_arg_as_ssize(r_obj* n, const char* arg) {
  switch (r_typeof(n)) {

  case R_TYPE_double: {
    if (r_length(n) != 1) {
      goto invalid;
    }

    double out = r_dbl_get(n, 0);

    if (out > R_SSIZE_MAX) {
      r_abort("`%s` is too large a number.", arg);
    }
    if (out != (int_least64_t) out) {
      r_abort("`%s` must be a whole number, not a decimal number.", arg);
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
    r_abort("`%s` must be a scalar integer or double.", arg);
  }
}

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

  // Local precious lists are disabled by default because rchk
  // requires the base precious list and we don't want to
  // double-preserve. Still enable it on CI to get that part of the
  // code tested.
  _r_use_local_precious_list =
    getenv("RLIB_USE_LOCAL_PRECIOUS_LIST") ||
    getenv("CI");

  // Need to be first
  r_init_library_vendor(); // Needed for xxh used in `r_preserve()`
  r_init_library_globals_syms();
  r_init_library_obj(ns);
  r_init_library_globals(ns);

  r_init_rlang_ns_env();
  r_init_library_arg();
  r_init_library_call();
  r_init_library_cnd();
  r_init_library_dyn_array();
  r_init_library_env();
  r_init_library_eval();
  r_init_library_fn();
  r_init_library_quo();
  r_init_library_session();
  r_init_library_sym();
  r_init_library_stack();

  shared_x_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_envs.base);
  r_preserve(shared_x_env);

  shared_xy_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_envs.base);
  r_preserve(shared_xy_env);

  shared_xyz_env = r_parse_eval("new.env(hash = FALSE, parent = baseenv(), size = 1L)", r_envs.base);
  r_preserve(shared_xyz_env);

  // Return a SEXP so the init function can be called from R
  return r_null;
}

bool _r_use_local_precious_list = false;
