#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include "export.h"

extern SEXP rlang_new_dictionary(SEXP x, SEXP lookup_msg, SEXP read_only);
extern SEXP rlang_squash_if(SEXP dots, SEXPTYPE kind, bool (*is_spliceable)(SEXP), int depth);
extern bool is_clevel_spliceable(SEXP x);

extern SEXP sym_tilde;

void R_init_rlang(DllInfo* dll) {
  R_RegisterCCallable("rlang", "rlang_new_dictionary", (DL_FUNC) &rlang_new_dictionary);
  R_RegisterCCallable("rlang", "rlang_squash_if", (DL_FUNC) &rlang_squash_if);

  rlang_register_pointer("rlang", "test_is_spliceable", (DL_FUNC) &is_clevel_spliceable);

  sym_tilde = Rf_install("~");
}
