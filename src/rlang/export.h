#ifndef RLANG_EXPORT_H
#define RLANG_EXPORT_H

#include <Rversion.h>
#include <R_ext/Rdynload.h>


#if (defined(R_VERSION) && R_VERSION < R_Version(3, 4, 0))
typedef union {void* p; DL_FUNC fn;} fn_ptr;
sexp* R_MakeExternalPtrFn(DL_FUNC p, sexp* tag, sexp* prot);
DL_FUNC R_ExternalPtrAddrFn(sexp* s);
#endif

typedef DL_FUNC r_void_fn;
typedef R_CallMethodDef r_callable;
typedef R_ExternalMethodDef r_external;
typedef DllInfo r_dll_info;


static inline void r_register_c_callable(const char* pkg, const char* ptr_name, r_void_fn fn) {
  R_RegisterCCallable(pkg, ptr_name, fn);
}

static inline void r_register_r_callables(r_dll_info* dll,
                                          const r_callable* const callables,
                                          const r_external* const externals) {
  R_registerRoutines(dll, NULL, callables, NULL, externals);
  R_useDynamicSymbols(dll, FALSE);
}

static inline r_void_fn r_peek_c_callable(const char* pkg, const char* callable) {
  return R_GetCCallable(pkg, callable);
}


#endif
