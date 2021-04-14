#ifndef RLANG_CND_H
#define RLANG_CND_H

#include <stdbool.h>
#include "rlang.h"


void r_inform(const char* fmt, ...);
void r_warn(const char* fmt, ...);
void r_abort(const char* fmt, ...) __attribute__((noreturn));
void r_interrupt();

__attribute__((noreturn))
void r_stop_internal(const char* fn, const char* fmt, ...);

static inline
__attribute__((noreturn))
void r_stop_unreached(const char* fn) {
  r_stop_internal(fn, "Reached the unreachable.");
}

static inline
__attribute__((noreturn))
void r_stop_unimplemented_type(const char* fn, enum r_type type) {
  r_stop_internal(fn, "Unimplemented type `%s`.", Rf_type2char(type));
}
static inline
__attribute__((noreturn))
void r_stop_unexpected_type(const char* fn, enum r_type type) {
  r_stop_internal(fn, "Unexpected type `%s`.", Rf_type2char(type));
}


static inline
bool r_is_condition(r_obj* x) {
  return r_typeof(x) == R_TYPE_list && r_inherits(x, "condition");
}
r_obj* r_new_condition(r_obj* type, r_obj* msg, r_obj* data);

void r_cnd_signal(r_obj* cnd);
void r_cnd_inform(r_obj* cnd, bool mufflable);
void r_cnd_warn(r_obj* cnd, bool mufflable);
void r_cnd_abort(r_obj* cnd, bool mufflable);

enum r_condition_type {
  r_cnd_type_condition = 0,
  r_cnd_type_message = 1,
  r_cnd_type_warning = 2,
  r_cnd_type_error = 3,
  r_cnd_type_interrupt = 4
};

enum r_condition_type r_cnd_type(r_obj* cnd);


#endif
