#include "rlang.h"

int r_as_optional_bool(sexp* lgl) {
  if (lgl == r_null) {
    return -1;
  } else {
    return r_lgl_get(lgl, 0);
  }
}

bool r_is_true(sexp* x) {
  if (!r_is_scalar_logical(x)) {
    return false;
  } else {
    int value = LOGICAL(x)[0];
    return value == NA_LOGICAL ? 0 : value;
  }
}

r_ssize r_lgl_sum(sexp* x, bool na_true) {
  if (r_typeof(x) != r_type_logical) {
    r_abort("Internal error: Excepted logical vector in `r_lgl_sum()`");
  }

  r_ssize n = r_vec_length(x);

  r_ssize sum = 0;
  int* ptr = r_lgl_deref(x);

  for (r_ssize i = 0; i < n; ++i, ++ptr) {
    // This can't overflow since `sum` is necessarily smaller or equal
    // to the vector length expressed in `r_ssize`.
    if (na_true && *ptr) {
      sum += 1;
    } else if (*ptr == 1) {
      sum += 1;
    }
  }

  return sum;
}

sexp* r_lgl_which(sexp* x, bool na_propagate) {
  if (r_typeof(x) != r_type_logical) {
    r_abort("Internal error: Expected logical vector in `r_lgl_which()`");
  }

  r_ssize n = r_length(x);
  int* data = r_lgl_deref(x);

  r_ssize which_n = r_lgl_sum(x, na_propagate);

  if (which_n > INT_MAX) {
    r_abort("Internal error: Can't fit result of `r_lgl_which()` in an integer vector");
  }

  sexp* which = KEEP(r_new_vector(r_type_integer, which_n));
  int* which_data = r_int_deref(which);

  for (int i = 0; i < n; ++i, ++data) {
    int elt = *data;

    if (elt) {
      if (na_propagate && elt == NA_LOGICAL) {
        *which_data = NA_INTEGER;
        ++which_data;
      } else if (elt != NA_LOGICAL) {
        *which_data = i + 1;
        ++which_data;
      }
    }
  }

  FREE(1);
  return which;
}
