#include "rlang.h"

r_ssize r_lgl_sum(r_obj* x, bool na_true) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort("Internal error: Excepted logical vector in `r_lgl_sum()`");
  }

  r_ssize n = r_length(x);

  r_ssize sum = 0;
  const int* p_x = r_lgl_cbegin(x);

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

r_obj* r_lgl_which(r_obj* x, bool na_propagate) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort("Internal error: Expected logical vector in `r_lgl_which()`");
  }

  r_ssize n = r_length(x);
  const int* p_x = r_lgl_cbegin(x);

  r_ssize which_n = r_lgl_sum(x, na_propagate);

  if (which_n > INT_MAX) {
    r_abort("Internal error: Can't fit result of `r_lgl_which()` in an integer vector");
  }

  r_obj* which = KEEP(r_alloc_integer(which_n));
  int* p_which = r_int_begin(which);

  for (int i = 0; i < n; ++i) {
    int elt = p_x[i];

    if (elt) {
      if (na_propagate && elt == r_globals.na_lgl) {
        *p_which = r_globals.na_int;
        ++p_which;
      } else if (elt != r_globals.na_lgl) {
        *p_which = i + 1;
        ++p_which;
      }
    }
  }

  FREE(1);
  return which;
}
