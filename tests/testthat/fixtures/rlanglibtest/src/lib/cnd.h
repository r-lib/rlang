#ifndef RLANG_CND_H
#define RLANG_CND_H

#include <stdbool.h>


void r_inform(const char* fmt, ...);
void r_warn(const char* fmt, ...);
void r_abort(const char* fmt, ...) __attribute__((noreturn));

sexp* r_interp_str(const char* fmt, ...);

sexp* r_new_condition(sexp* type, sexp* data, sexp* msg);

static inline bool r_is_condition(sexp* x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "condition");
}

void r_cnd_signal(sexp* cnd, bool mufflable);
void r_cnd_inform(sexp* cnd, bool mufflable);
void r_cnd_warn(sexp* cnd, bool mufflable);
void r_cnd_abort(sexp* cnd, bool mufflable);


#endif
