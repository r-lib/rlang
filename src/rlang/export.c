#include "rlang.h"
#include "export.h"
#include <Rversion.h>

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
  SEXP ns_string = KEEP(Rf_mkString(ns));
  SEXP call = KEEP(r_sym("getNamespace"));
  call = KEEP(Rf_lang2(call, ns_string));
  SEXP ns_env = r_eval(call, R_BaseEnv);
  FREE(3);
  return ns_env;
}

void rlang_register_pointer(const char* ns, const char* ptr_name, DL_FUNC fn) {
  SEXP ptr = KEEP(R_MakeExternalPtrFn(fn, r_null, r_null));

  SEXP ptr_obj = KEEP(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(ptr_obj, 0, ptr);

  SEXP ptr_class = KEEP(Rf_mkString("fn_pointer"));
  Rf_setAttrib(ptr_obj, R_ClassSymbol, ptr_class);

  SEXP ns_env = KEEP(rlang_namespace(ns));
  Rf_defineVar(r_sym(ptr_name), ptr_obj, ns_env);
  FREE(4);
}
