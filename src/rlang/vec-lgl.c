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
  const enum r_type type = r_typeof(x);

  if (type != R_TYPE_logical) {
    r_stop_unexpected_type(type);
  }

  const r_ssize n = r_length(x);
  const int* v_x = r_lgl_cbegin(x);

  r_obj* names = r_names(x);
  const bool has_names = names != r_null;
  r_obj* const* v_names = NULL;
  if (has_names) {
    v_names = r_chr_cbegin(names);
  }

  const r_ssize which_n = r_lgl_sum(x, na_propagate);

  if (which_n > INT_MAX) {
    r_stop_internal("Can't fit result in an integer vector.");
  }

  r_obj* which = KEEP(r_alloc_integer(which_n));
  int* v_which = r_int_begin(which);

  r_obj* which_names = r_null;
  if (has_names) {
    which_names = r_alloc_character(which_n);
    r_attrib_poke_names(which, which_names);
  }

  r_ssize j = 0;

  if (na_propagate) {
    for (r_ssize i = 0; i < n; ++i) {
      const int elt = v_x[i];

      if (elt != 0) {
        v_which[j] = (elt == r_globals.na_lgl) ? r_globals.na_int : i + 1;

        if (has_names) {
          r_chr_poke(which_names, j, v_names[i]);
        }

        ++j;
      }
    }
  } else {
    for (r_ssize i = 0; i < n; ++i) {
      const int elt = v_x[i];

      if (elt == 1) {
        v_which[j] = i + 1;

        if (has_names) {
          r_chr_poke(which_names, j, v_names[i]);
        }

        ++j;
      }
    }
  }

  FREE(1);
  return which;
}
