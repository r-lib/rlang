#include <rlang.h>
#include <math.h>

enum is_number {
  IS_NUMBER_true = 0,
  IS_NUMBER_false = 1,
  IS_NUMBER_oob = 2
};

#include "decl/standalone-types-check-decl.h"


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

r_obj* ffi_standalone_check_number(r_obj* x,
                                   r_obj* allow_decimal,
                                   r_obj* min,
                                   r_obj* max,
                                   r_obj* allow_infinite,
                                   r_obj* allow_na,
                                   r_obj* allow_null) {
  int out = IS_NUMBER_false;

  switch (r_typeof(x)) {
  case R_TYPE_null:
    out = r_as_bool(allow_null) ? IS_NUMBER_true : IS_NUMBER_false;
    break;

  case R_TYPE_logical:
    if (r_length(x) == 1 && r_lgl_get(x, 0) == r_globals.na_lgl) {
      out = r_as_bool(allow_na) ? IS_NUMBER_true : IS_NUMBER_false;
    }
    break;

  case R_TYPE_integer:
    out = int_standalone_check_number(x,
                                      min,
                                      max,
                                      allow_na,
                                      allow_null);
    break;

  case R_TYPE_double:
    out = dbl_standalone_check_number(x,
                                      allow_decimal,
                                      min,
                                      max,
                                      allow_infinite,
                                      allow_na,
                                      allow_null);
    break;

  default:
    break;
  }

  return r_int(out);
}

static
bool is_numeric(r_obj* x) {
  if (r_attrib_get(x, r_syms.class_) == r_null) {
    return true;
  }

  r_obj* call = KEEP(r_call2(r_sym("is.numeric"), x));
  r_obj* ffi_out = r_eval(call, r_envs.base);

  bool out = r_as_bool(ffi_out);

  FREE(1);
  return out;
}

static
enum is_number int_standalone_check_number(r_obj* x,
                                           r_obj* ffi_min,
                                           r_obj* ffi_max,
                                           r_obj* allow_na,
                                           r_obj* allow_null) {
  if (r_length(x) != 1) {
    return IS_NUMBER_false;
  }
  if (!is_numeric(x)) {
    return IS_NUMBER_false;
  }

  int value = r_int_get(x, 0);

  if (value == r_globals.na_int) {
    return r_as_bool(allow_na) ? IS_NUMBER_true : IS_NUMBER_false;
  }

  double min = r_arg_as_double(ffi_min, "min");
  double max = r_arg_as_double(ffi_max, "max");

  if (value < min || value > max) {
    return IS_NUMBER_oob;
  }

  return IS_NUMBER_true;
}

static
enum is_number dbl_standalone_check_number(r_obj* x,
                                           r_obj* allow_decimal,
                                           r_obj* ffi_min,
                                           r_obj* ffi_max,
                                           r_obj* allow_infinite,
                                           r_obj* allow_na,
                                           r_obj* allow_null) {
  if (r_length(x) != 1) {
    return IS_NUMBER_false;
  }
  if (!is_numeric(x)) {
    return IS_NUMBER_false;
  }

  double value = r_dbl_get(x, 0);

  // `value == r_globals.na_dbl` is always false since it involves NaN
  if (memcmp(&value, &r_globals.na_dbl, sizeof(double)) == 0) {
    return r_as_bool(allow_na) ? IS_NUMBER_true : IS_NUMBER_false;
  }

  if (!isfinite(value)) {
    if (isnan(value)) {
      return IS_NUMBER_false;
    } else {
      return r_as_bool(allow_infinite) ? IS_NUMBER_true : IS_NUMBER_false;
    }
  }

  if (!r_as_bool(allow_decimal) && !r_dbl_is_decimal(value)) {
    return IS_NUMBER_false;
  }

  double min = r_arg_as_double(ffi_min, "min");
  double max = r_arg_as_double(ffi_max, "max");

  if (value < min || value > max) {
    return IS_NUMBER_oob;
  }

  return IS_NUMBER_true;
}
