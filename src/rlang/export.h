// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_EXPORT_H
#define RLANG_EXPORT_H

#include "rlang-types.h"

typedef DL_FUNC r_void_fn;

static inline
r_void_fn r_peek_c_callable(const char* pkg, const char* callable) {
  return R_GetCCallable(pkg, callable);
}

static inline
r_obj* r_new_fn_ptr(r_void_fn p) {
  return R_MakeExternalPtrFn(p, r_null, r_null);
}

static inline
r_void_fn r_fn_ptr_addr(r_obj* p) {
  return R_ExternalPtrAddrFn(p);
}


#endif
