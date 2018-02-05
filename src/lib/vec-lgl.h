#ifndef RLANG_VECTOR_LGL_H
#define RLANG_VECTOR_LGL_H


bool r_as_bool(sexp* x);
int r_as_optional_bool(sexp* lgl);

bool r_is_true(sexp* x);

static inline sexp* r_bool_as_shared_logical(bool x) {
  if (x) {
    return r_shared_true;
  } else {
    return r_shared_false;
  }
}


#endif
