#define R_NO_REMAP
#include <Rinternals.h>
#include <Rversion.h>

#include "export.h"

#if (defined(R_VERSION) && R_VERSION < R_Version(3, 4, 0))
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot) {
  fn_ptr ptr;
  ptr.fn = p;
  return R_MakeExternalPtr(ptr.p, tag, prot);
}
DL_FUNC R_ExternalPtrAddrFn(SEXP s) {
  fn_ptr ptr;
  ptr.p = EXTPTR_PTR(s);
  return ptr.fn;
}
#endif

SEXP rlang_namespace(const char* ns) {
  SEXP ns_string = PROTECT(Rf_mkString(ns));
  SEXP call = PROTECT(Rf_install("getNamespace"));
  call = PROTECT(Rf_lang2(call, ns_string));
  SEXP ns_env = Rf_eval(call, R_BaseEnv);
  UNPROTECT(3);
  return ns_env;
}

void rlang_register_pointer(const char* ns, const char* ptr_name, DL_FUNC fn) {
  SEXP ptr = PROTECT(R_MakeExternalPtrFn(fn, R_NilValue, R_NilValue));

  SEXP ptr_obj = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(ptr_obj, 0, ptr);

  SEXP ptr_class = PROTECT(Rf_mkString("fn_pointer"));
  Rf_setAttrib(ptr_obj, R_ClassSymbol, ptr_class);

  SEXP ns_env = PROTECT(rlang_namespace(ns));
  Rf_defineVar(Rf_install(ptr_name), ptr_obj, ns_env);
  UNPROTECT(4);
}
