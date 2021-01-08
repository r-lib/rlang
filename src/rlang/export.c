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
