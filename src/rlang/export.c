#include "rlang.h"
#include "export.h"
#include <Rversion.h>

#if (defined(R_VERSION) && R_VERSION < R_Version(3, 4, 0))
r_obj* R_MakeExternalPtrFn(DL_FUNC p, r_obj* tag, r_obj* prot) {
  fn_ptr ptr;
  ptr.fn = p;
  return R_MakeExternalPtr(ptr.p, tag, prot);
}
DL_FUNC R_ExternalPtrAddrFn(r_obj* s) {
  fn_ptr ptr;
  ptr.p = EXTPTR_PTR(s);
  return ptr.fn;
}
#endif

r_obj* rlang_namespace(const char* ns) {
  r_obj* ns_string = KEEP(Rf_mkString(ns));
  r_obj* call = KEEP(r_sym("getNamespace"));
  call = KEEP(Rf_lang2(call, ns_string));
  r_obj* ns_env = r_eval(call, R_BaseEnv);
  FREE(3);
  return ns_env;
}
