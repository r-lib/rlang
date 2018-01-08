#include "rlang.h"
#include "export.h"
#include <Rversion.h>

#if (defined(R_VERSION) && R_VERSION < R_Version(3, 4, 0))
sexp* R_MakeExternalPtrFn(DL_FUNC p, sexp* tag, sexp* prot) {
  fn_ptr ptr;
  ptr.fn = p;
  return R_MakeExternalPtr(ptr.p, tag, prot);
}
DL_FUNC R_ExternalPtrAddrFn(sexp* s) {
  fn_ptr ptr;
  ptr.p = EXTPTR_PTR(s);
  return ptr.fn;
}
#endif

sexp* rlang_namespace(const char* ns) {
  sexp* ns_string = KEEP(Rf_mkString(ns));
  sexp* call = KEEP(r_sym("getNamespace"));
  call = KEEP(Rf_lang2(call, ns_string));
  sexp* ns_env = r_eval(call, R_BaseEnv);
  FREE(3);
  return ns_env;
}

void rlang_register_pointer(const char* ns, const char* ptr_name, DL_FUNC fn) {
  sexp* ptr = KEEP(R_MakeExternalPtrFn(fn, r_null, r_null));

  sexp* ptr_obj = KEEP(r_new_vector(VECSXP, 1));
  SET_VECTOR_ELT(ptr_obj, 0, ptr);

  sexp* ptr_class = KEEP(Rf_mkString("fn_pointer"));
  Rf_setAttrib(ptr_obj, R_ClassSymbol, ptr_class);

  sexp* ns_env = KEEP(rlang_namespace(ns));
  Rf_defineVar(r_sym(ptr_name), ptr_obj, ns_env);
  FREE(4);
}
