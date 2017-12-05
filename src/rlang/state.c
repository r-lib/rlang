#include "rlang.h"


sexp* r_peek_option(const char* name) {
  return Rf_GetOption1(r_sym(name));
}
