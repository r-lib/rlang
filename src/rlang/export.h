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

static inline r_void_fn r_peek_c_callable(const char* pkg, const char* callable) {
  return R_GetCCallable(pkg, callable);
}


#endif
