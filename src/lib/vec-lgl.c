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

r_ssize r_lgl_sum(sexp* lgl) {
  if (r_typeof(lgl) != r_type_logical) {
    r_abort("Internal error: Excepted logical vector for sum");
  }

  r_ssize n = r_vec_length(lgl);

  r_ssize sum = 0;
  int* ptr = r_lgl_deref(lgl);

  for (r_ssize i = 0; i < n; ++i, ++ptr) {
    // This can't overflow since `sum` is necessarily smaller or equal
    // to the vector length expressed in `r_ssize`.
    sum += *ptr;
  }

  return sum;
}
