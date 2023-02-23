#include "rlang.h"
#include <stdlib.h>

r_ssize r_lgl_sum(r_obj* x, bool na_true) {
  if (r_typeof(x) != R_TYPE_logical) {
    r_abort("Internal error: Excepted logical vector in `r_lgl_sum()`");
  }

  const r_ssize n = r_length(x);
  const int* v_x = r_lgl_cbegin(x);

  // This can't overflow since `sum` is necessarily smaller or equal
  // to the vector length expressed in `r_ssize`
  r_ssize sum = 0;

  if (na_true) {
    for (r_ssize i = 0; i < n; ++i) {
      sum += (bool) v_x[i];
    }
  } else {
    for (r_ssize i = 0; i < n; ++i) {
      sum += (v_x[i] == 1);
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

  const r_ssize out_n = r_lgl_sum(x, na_propagate);

  if (out_n > INT_MAX) {
    r_stop_internal("Can't fit result in an integer vector.");
  }

  r_obj* out = KEEP(r_alloc_integer(out_n));
  int* v_out = r_int_begin(out);

  r_obj* names = r_names(x);
  const bool has_names = (names != r_null);

  if (na_propagate) {
    if (has_names) {
      // Mark `NA` locations with negative location for extracting names later
      for (r_ssize i = 0, j = 0; i < n && j < out_n; ++i) {
        const int x_elt = v_x[i];
        const bool missing = x_elt == r_globals.na_lgl;
        const int elt = missing * (-i - 1) + !missing * x_elt * (i + 1);
        v_out[j] = elt;
        j += (bool) elt;
      }
    } else {
      for (r_ssize i = 0, j = 0; i < n && j < out_n; ++i) {
        const int x_elt = v_x[i];
        const bool missing = x_elt == r_globals.na_lgl;
        const int elt = missing * r_globals.na_int + !missing * x_elt * (i + 1);
        v_out[j] = elt;
        j += (bool) elt;
      }
    }
  } else {
    for (r_ssize i = 0, j = 0; i < n && j < out_n; ++i) {
      const int x_elt = v_x[i];
      v_out[j] = i + 1;
      j += (x_elt == 1);
    }
  }

  if (has_names) {
    r_obj* const* v_names = r_chr_cbegin(names);

    r_obj* out_names = r_alloc_character(out_n);
    r_attrib_poke_names(out, out_names);

    if (na_propagate) {
      // `v_out` contains negative locations which tells you the location of the
      // name to extract while also serving as a signal of where `NA`s should go
      // in the finalized output
      for (r_ssize i = 0; i < out_n; ++i) {
        const int loc = v_out[i];
        const int abs_loc = abs(loc);
        const bool same = (loc == abs_loc);
        v_out[i] = same * loc + !same * r_globals.na_int;
        r_chr_poke(out_names, i, v_names[abs_loc - 1]);
      }
    } else {
      // `v_out` doesn't contain `NA`, so we can use the locations directly
      for (r_ssize i = 0; i < out_n; ++i) {
        const int loc = v_out[i] - 1;
        r_chr_poke(out_names, i, v_names[loc]);
      }
    }
  }

  FREE(1);
  return out;
}
