#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include "export.h"

extern SEXP rlang_new_dictionary(SEXP x, SEXP lookup_msg, SEXP read_only);
extern bool is_clevel_spliceable(SEXP x);

void R_init_rlang(DllInfo* dll) {
  R_RegisterCCallable("rlang", "rlang_new_dictionary", (DL_FUNC) &rlang_new_dictionary);

  rlang_register_pointer("rlang", "test_is_spliceable", (DL_FUNC) &is_clevel_spliceable);
}
