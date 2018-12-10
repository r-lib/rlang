#ifndef RLANG_CND_H
#define RLANG_CND_H

#include <stdbool.h>


void r_inform(const char* fmt, ...);
void r_warn(const char* fmt, ...);
void r_abort(const char* fmt, ...) __attribute__((noreturn));
void r_interrupt();

void r_signal_soft_deprecated(const char* msg, const char* id, sexp* env);
void r_warn_deprecated(const char* id, const char* fmt, ...);
void r_stop_defunct(const char* fmt, ...);

sexp* r_interp_str(const char* fmt, ...);

sexp* r_new_condition(sexp* type, sexp* msg, sexp* data);

static inline bool r_is_condition(sexp* x) {
  return TYPEOF(x) == VECSXP && Rf_inherits(x, "condition");
}

void r_cnd_signal(sexp* cnd);
void r_cnd_inform(sexp* cnd, bool mufflable);
void r_cnd_warn(sexp* cnd, bool mufflable);
void r_cnd_abort(sexp* cnd, bool mufflable);

enum r_condition_type {
  r_cnd_type_condition = 0,
  r_cnd_type_message = 1,
  r_cnd_type_warning = 2,
  r_cnd_type_error = 3,
  r_cnd_type_interrupt = 4
};

enum r_condition_type r_cnd_type(sexp* cnd);


#endif
