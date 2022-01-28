#include <rlang.h>
#include "vec.h"

static r_obj* replace_na_(r_obj* x, r_obj* replacement, int start);
static r_obj* replace_na_vec_(r_obj* x, r_obj* replacement, int start);

r_obj* ffi_replace_na(r_obj* x, r_obj* replacement) {
  const enum r_type x_type = r_typeof(x);
  const enum r_type replacement_type = r_typeof(replacement);

  int n = r_length(x);
  int n_replacement = r_length(replacement);

  if (!r_is_atomic(x, -1)) {
    r_abort("Cannot replace missing values in an object of type %s", Rf_type2char(x_type));
  }

  if (x_type != replacement_type) {
    r_abort("Replacement values must have type %s, not type %s", Rf_type2char(x_type), Rf_type2char(replacement_type));
  }

  if (n_replacement != 1 && n_replacement != n) {
    if (n == 1) {
      r_abort("The replacement values must have size 1, not %i", n_replacement);
    } else {
      r_abort("The replacement values must have size 1 or %i, not %i", n, n_replacement);
    }
  }

  int i = 0;

  switch(x_type) {
  case R_TYPE_logical: {
    int* arr = r_lgl_begin(x);
    for (; i < n; ++i) {
      if (arr[i] == r_globals.na_lgl) {
        break;
      }
    }
    break;
  }

  case R_TYPE_integer: {
    int* arr = r_int_begin(x);
    for (; i < n; ++i) {
      if (arr[i] == r_globals.na_int) {
        break;
      }
    }
    break;
  }

  case R_TYPE_double: {
    double* arr = r_dbl_begin(x);
    for (; i < n; ++i) {
      if (ISNA(arr[i])) {
        break;
      }
    }
    break;
  }

  case R_TYPE_character: {
    for (; i < n; ++i) {
      if (r_chr_get(x, i) == r_globals.na_str) {
        break;
      }
    }
    break;
  }

  case R_TYPE_complex: {
    r_complex* arr = r_cpl_begin(x);

    for (; i < n; ++i) {
      if (ISNA(arr[i].r)) {
        break;
      }
    }
    break;
  }

  default: {
    r_abort("Internal error: Don't know how to handle object of type %s", Rf_type2char(x_type));
  }
  }

  if (i == n) {
    return x;
  } else if (n_replacement == 1) {
    return replace_na_(x, replacement, i);
  } else {
    return replace_na_vec_(x, replacement, i);
  }
}

static r_obj* replace_na_(r_obj* x, r_obj* replacement, int i) {
  KEEP(x = r_copy(x));
  int n = r_length(x);

  switch(r_typeof(x)) {
  case R_TYPE_logical: {
    int* arr = r_lgl_begin(x);
    int new_value = r_lgl_begin(replacement)[0];
    for (; i < n; ++i) {
      if (arr[i] == r_globals.na_lgl) {
        arr[i] = new_value;
      }
    }
    break;
  }

  case R_TYPE_integer: {
    int* arr = r_int_begin(x);
    int new_value = r_int_begin(replacement)[0];
    for (; i < n; ++i) {
      if (arr[i] == r_globals.na_int) {
        arr[i] = new_value;
      }
    }
    break;
  }

  case R_TYPE_double: {
    double* arr = r_dbl_begin(x);
    double new_value = r_dbl_begin(replacement)[0];
    for (; i < n; ++i) {
      if (ISNA(arr[i])) {
        arr[i] = new_value;
      }
    }
    break;
  }

  case R_TYPE_character: {
    r_obj* new_value = r_chr_get(replacement, 0);
    for (; i < n; ++i) {
      if (r_chr_get(x, i) == r_globals.na_str) {
        r_chr_poke(x, i, new_value);
      }
    }
    break;
  }

  case R_TYPE_complex: {
    r_complex* arr = r_cpl_begin(x);
    r_complex new_value = r_cpl_get(replacement, 0);

    for (; i < n; ++i) {
      if (ISNA(arr[i].r)) {
        arr[i] = new_value;
      }
    }
    break;
  }

  default: {
    r_abort("Internal error: Don't know how to handle object of type %s", Rf_type2char(r_typeof(x)));
  }
  }

  FREE(1);
  return x;
}


static r_obj* replace_na_vec_(r_obj* x, r_obj* replacement, int i) {
  KEEP(x = r_copy(x));
  int n = r_length(x);

  switch(r_typeof(x)) {
  case R_TYPE_logical: {
    int* arr = r_lgl_begin(x);
    for (; i < n; ++i) {
      if (arr[i] == r_globals.na_lgl) {
        arr[i] = r_lgl_get(replacement, i);
      }
    }
    break;
  }

  case R_TYPE_integer: {
    int* arr = r_int_begin(x);
    for (; i < n; ++i) {
      if (arr[i] == r_globals.na_int) {
        arr[i] = r_int_get(replacement, i);
      }
    }
    break;
  }

  case R_TYPE_double: {
    double* arr = r_dbl_begin(x);
    for (; i < n; ++i) {
      if (ISNA(arr[i])) {
        arr[i] = r_dbl_get(replacement, i);
      }
    }
    break;
  }

  case R_TYPE_character: {
    for (; i < n; ++i) {
      if (r_chr_get(x, i) == r_globals.na_str) {
        r_chr_poke(x, i, r_chr_get(replacement, i));
      }
    }
    break;
  }

  case R_TYPE_complex: {
    r_complex* arr = r_cpl_begin(x);
    for (; i < n; ++i) {
      if (ISNA(arr[i].r)) {
        arr[i] = r_cpl_get(replacement, i);
      }
    }
    break;
  }

  default: {
    r_abort("Internal error: Don't know how to handle object of type %s", Rf_type2char(r_typeof(x)));
  }
  }

  FREE(1);
  return x;
}
