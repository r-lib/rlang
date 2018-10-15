#ifndef RLANG_VECTOR_LGL_H
#define RLANG_VECTOR_LGL_H


int r_as_optional_bool(sexp* lgl);

static inline sexp* r_lgl(bool x) {
  return Rf_ScalarLogical(x);
}

bool r_is_true(sexp* x);

static inline sexp* r_shared_lgl(bool x) {
  if (x) {
    return r_shared_true;
  } else {
    return r_shared_false;
  }
}

r_ssize r_lgl_sum(sexp* lgl);


#endif
