#ifndef RLANG_CND_H
#define RLANG_CND_H

#include <stdbool.h>


void r_inform(const char* fmt, ...);
void r_warn(const char* fmt, ...);
void r_abort(const char* fmt, ...) __attribute__((noreturn));

SEXP r_interp_str(const char* fmt, ...);

SEXP r_new_condition(SEXP type, SEXP data, SEXP msg);

static inline bool r_is_condition(SEXP x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "condition");
}

void r_cnd_signal(SEXP cnd, bool mufflable);
void r_cnd_inform(SEXP cnd, bool mufflable);
void r_cnd_warn(SEXP cnd, bool mufflable);
void r_cnd_abort(SEXP cnd, bool mufflable);


#endif
