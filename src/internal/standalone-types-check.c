#include <rlang.h>

r_obj* ffi_standalone_is_bool(r_obj* x,
                              r_obj* allow_na,
                              r_obj* allow_null) {
  if (x == r_null) {
    return r_lgl(r_as_bool(allow_null));
  }

  if (r_typeof(x) != R_TYPE_logical || r_length(x) != 1) {
    return r_false;
  }

  if (r_lgl_get(x, 0) == r_globals.na_lgl) {
    return r_lgl(r_as_bool(allow_na));
  }

  return r_true;
}
