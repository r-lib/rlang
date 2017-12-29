#ifndef RLANG_EXPORT_H
#define RLANG_EXPORT_H

#define R_NO_REMAP
#include <Rinternals.h>
#include <Rversion.h>
#include <R_ext/Rdynload.h>


#if (defined(R_VERSION) && R_VERSION < R_Version(3, 4, 0))
typedef union {void* p; DL_FUNC fn;} fn_ptr;
sexp* R_MakeExternalPtrFn(DL_FUNC p, sexp* tag, sexp* prot);
DL_FUNC R_ExternalPtrAddrFn(sexp* s);
#endif

void rlang_register_pointer(const char* ns, const char* ptr_name, DL_FUNC fn);


typedef DL_FUNC r_fn_ptr;

static inline void r_register_c_callable(const char* ns, const char* ptr_name, r_fn_ptr fn) {
  R_RegisterCCallable(ns, ptr_name, fn);
}

typedef R_CallMethodDef r_callable;
typedef DllInfo r_dll_info;

static inline void r_register_r_callables(r_dll_info* dll, const r_callable* const callables) {
  R_registerRoutines(dll, NULL, callables, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}


#endif
