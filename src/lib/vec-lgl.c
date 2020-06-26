#include "rlang.h"

int r_as_optional_bool(sexp* lgl) {
  if (lgl == r_null) {
    return -1;
  } else {
    return r_lgl_get(lgl, 0);
  }
}

bool r_is_true(sexp* x) {
  if (!r_is_bool(x)) {
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
  const int* p_x = r_lgl_deref_const(x);

  for (r_ssize i = 0; i < n; ++i) {
    // This can't overflow since `sum` is necessarily smaller or equal
    // to the vector length expressed in `r_ssize`.
    int x_i = p_x[i];

    if (na_true && x_i) {
      sum += 1;
    } else if (x_i == 1) {
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
  const int* p_x = r_lgl_deref_const(x);

  r_ssize which_n = r_lgl_sum(x, na_propagate);

  if (which_n > INT_MAX) {
    r_abort("Internal error: Can't fit result of `r_lgl_which()` in an integer vector");
  }

  sexp* which = KEEP(r_new_vector(r_type_integer, which_n));
  int* p_which = r_int_deref(which);

  for (int i = 0; i < n; ++i) {
    int elt = p_x[i];

    if (elt) {
      if (na_propagate && elt == NA_LOGICAL) {
        *p_which = NA_INTEGER;
        ++p_which;
      } else if (elt != NA_LOGICAL) {
        *p_which = i + 1;
        ++p_which;
      }
    }
  }

  FREE(1);
  return which;
}
