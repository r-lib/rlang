#include "rlang.h"

SEXP r_sym(const char* c_string) {
  return Rf_install(c_string);
}
