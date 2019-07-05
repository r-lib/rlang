#include "rlang.h"

static sexp* replace_na_(sexp* x, sexp* replacement, int start);
static sexp* replace_na_vec_(sexp* x, sexp* replacement, int start);

sexp* rlang_replace_na(sexp* x, sexp* replacement) {
  int n = r_length(x);
  int i = 0;

  switch(r_typeof(x)) {
  case LGLSXP: {
    int* arr = LOGICAL(x);
    for (; i < n; ++i) {
      if (arr[i] == NA_LOGICAL) {
        break;
      }
    }
    break;
  }

  case INTSXP: {
    int* arr = INTEGER(x);
    for (; i < n; ++i) {
      if (arr[i] == NA_INTEGER) {
        break;
      }
    }
    break;
  }

  case REALSXP: {
    double* arr = REAL(x);
    for (; i < n; ++i) {
      if (ISNA(arr[i])) {
        break;
      }
    }
    break;
  }

  case STRSXP: {
    for (; i < n; ++i) {
      if (STRING_ELT(x, i) == NA_STRING) {
        break;
      }
    }
    break;
  }

  case CPLXSXP: {
    r_complex_t* arr = COMPLEX(x);

    for (; i < n; ++i) {
      if (ISNA(arr[i].r)) {
        break;
      }
    }
    break;
  }

  default: {
    r_abort("Don't know how to handle object of type", Rf_type2char(r_typeof(x)));
  }
  }

  if (i == n) {
    return x;
  } else if (r_length(replacement) == 1) {
    return replace_na_(x, replacement, i);
  } else {
    return replace_na_vec_(x, replacement, i);
  }
}

static sexp* replace_na_(sexp* x, sexp* replacement, int i) {
  KEEP(x = Rf_duplicate(x));
  int n = r_length(x);

  switch(r_typeof(x)) {
  case LGLSXP: {
    int* arr = LOGICAL(x);
    int new_value = LOGICAL(replacement)[0];
    for (; i < n; ++i) {
      if (arr[i] == NA_LOGICAL) {
        arr[i] = new_value;
      }
    }
    break;
  }

  case INTSXP: {
    int* arr = INTEGER(x);
    int new_value = INTEGER(replacement)[0];
    for (; i < n; ++i) {
      if (arr[i] == NA_INTEGER) {
        arr[i] = new_value;
      }
    }
    break;
  }

  case REALSXP: {
    double* arr = REAL(x);
    double new_value = REAL(replacement)[0];
    for (; i < n; ++i) {
      if (ISNA(arr[i])) {
        arr[i] = new_value;
      }
    }
    break;
  }

  case STRSXP: {
    sexp* new_value = STRING_ELT(replacement, 0);
    for (; i < n; ++i) {
      if (STRING_ELT(x, i) == NA_STRING) {
        SET_STRING_ELT(x, i, new_value);
      }
    }
    break;
  }

  case CPLXSXP: {
    r_complex_t* arr = COMPLEX(x);
    r_complex_t new_value = COMPLEX(replacement)[0];

    for (; i < n; ++i) {
      if (ISNA(arr[i].r)) {
        arr[i] = new_value;
      }
    }
    break;
  }

  default: {
    r_abort("Don't know how to handle object of type", Rf_type2char(r_typeof(x)));
  }
  }

  FREE(1);
  return x;
}


static sexp* replace_na_vec_(sexp* x, sexp* replacement, int i) {
  KEEP(x = Rf_duplicate(x));
  int n = r_length(x);

  switch(r_typeof(x)) {
  case LGLSXP: {
    int* arr = LOGICAL(x);
    for (; i < n; ++i) {
      if (arr[i] == NA_LOGICAL) {
        arr[i] = LOGICAL(replacement)[i];
      }
    }
    break;
  }

  case INTSXP: {
    int* arr = INTEGER(x);
    for (; i < n; ++i) {
      if (arr[i] == NA_INTEGER) {
        arr[i] = INTEGER(replacement)[i];
      }
    }
    break;
  }

  case REALSXP: {
    double* arr = REAL(x);
    for (; i < n; ++i) {
      if (ISNA(arr[i])) {
        arr[i] = REAL(replacement)[i];
      }
    }
    break;
  }

  case STRSXP: {
    for (; i < n; ++i) {
      if (STRING_ELT(x, i) == NA_STRING) {
        SET_STRING_ELT(x, i, STRING_ELT(replacement, i));
      }
    }
    break;
  }

  case CPLXSXP: {
    r_complex_t* arr = COMPLEX(x);
    for (; i < n; ++i) {
      if (ISNA(arr[i].r)) {
        arr[i] = COMPLEX(replacement)[i];
      }
    }
    break;
  }

  default: {
    r_abort("Don't know how to handle object of type", Rf_type2char(r_typeof(x)));
  }
  }

  FREE(1);
  return x;
}
