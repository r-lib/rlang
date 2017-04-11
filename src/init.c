#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rlang_new_dictionary(SEXP x, SEXP lookup_msg, SEXP read_only);

void R_init_rlang(DllInfo* dll) {
  R_RegisterCCallable("rlang", "rlang_new_dictionary", (DL_FUNC) &rlang_new_dictionary);
}
