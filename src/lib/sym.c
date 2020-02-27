#include <string.h>

#include "rlang.h"


// In old R versions `as.name()` does not translate to native which
// loses the encoding. This symbol constructor always translates.
sexp* r_new_symbol(sexp* x, int* err) {
  switch (r_typeof(x)) {
  case SYMSXP:
    return x;
  case STRSXP:
    if (r_length(x) == 1) {
      const char* string = Rf_translateChar(r_chr_get(x, 0));
      return r_sym(string);
    } // else fallthrough
  default: {
    if (err) {
      *err = -1;
      return r_null;
    } else {
      const char* type = r_type_as_c_string(r_typeof(x));
      r_abort("Can't create a symbol with a %s", type);
    }
  }}
}

bool r_is_symbol(sexp* x, const char* string) {
  if (r_typeof(x) != SYMSXP) {
    return false;
  } else {
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
  }
}

bool r_is_symbol_any(sexp* x, const char** strings, int n) {
  if (r_typeof(x) != SYMSXP) {
    return false;
  }

  const char* name = CHAR(PRINTNAME(x));

  for (int i = 0; i < n; ++i) {
    if (strcmp(name, strings[i]) == 0) {
      return true;
    }
  }

  return false;
}

bool r_is_special_op_sym(sexp* x) {
  if (r_typeof(x) != SYMSXP) {
    return false;
  }

  const char* name = CHAR(PRINTNAME(x));
  int len = strlen(name);

  return
    len > 2 &&
    name[0] == '%' &&
    name[len - 1] == '%';
}


sexp* r_dot_environment_sym;
sexp* r_function_sym;
sexp* r_srcref_sym;
sexp* r_tilde_sym;

sexp* r_w_sym;
sexp* r_x_sym;
sexp* r_y_sym;
sexp* r_z_sym;

sexp* r_dot_x_sym;
sexp* r_dot_y_sym;
sexp* r_dot_fn_sym;

void r_init_library_sym() {
  r_dot_environment_sym = r_sym(".Environment");
  r_function_sym = r_sym("function");
  r_srcref_sym = r_sym("srcref");
  r_tilde_sym = r_sym("~");

  r_w_sym = r_sym("w");
  r_x_sym = r_sym("x");
  r_y_sym = r_sym("y");
  r_z_sym = r_sym("z");

  r_dot_x_sym = r_sym(".x");
  r_dot_y_sym = r_sym(".y");
  r_dot_fn_sym = r_sym(".fn");
}
