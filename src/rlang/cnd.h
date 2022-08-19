#ifndef RLANG_CND_H
#define RLANG_CND_H

#include <stdbool.h>


void r_inform(const char* fmt, ...);
void r_warn(const char* fmt, ...);
void r_interrupt();
void r_no_return r_abort(const char* fmt, ...);
void r_no_return r_abort_n(const struct r_pair* args, int n);
void r_no_return r_abort_call(r_obj* call, const char* fmt, ...);

// Formats input as an argument, using cli if available. Returns a
// vmax-protected string.
extern
const char* (*r_format_error_arg)(r_obj* arg);

const char* r_format_lazy_error_arg(struct r_lazy arg);

// Return vmax-protected strings
extern
const char* (*r_obj_type_friendly_full)(r_obj* x, bool value, bool length);

static inline
const char* r_obj_type_friendly(r_obj* x) {
  return r_obj_type_friendly_full(x, true, false);
}


extern
r_no_return
void (*r_stop_internal)(const char* file,
                        int line,
                        r_obj* call,
                        const char* fmt,
                        ...);

r_obj* r_peek_frame();

#define r_stop_internal(...)                            \
  (r_stop_internal)(__FILE__, __LINE__, r_peek_frame(), \
                    __VA_ARGS__)

#define r_stop_unreachable()                            \
  (r_stop_internal)(__FILE__, __LINE__, r_peek_frame(), \
                    "Reached the unreachable")

#define r_stop_unimplemented_type(TYPE)                                 \
  (r_stop_internal)(__FILE__, __LINE__, r_peek_frame(),                 \
                    "Unimplemented type `%s`.", Rf_type2char(TYPE))


#define r_stop_unexpected_type(TYPE)                                    \
  (r_stop_internal)(__FILE__, __LINE__, r_peek_frame(),                 \
                    "Unexpected type `%s`.", Rf_type2char(TYPE))


static inline
bool r_is_condition(r_obj* x) {
  return r_typeof(x) == R_TYPE_list && r_inherits(x, "condition");
}

void r_cnd_signal(r_obj* cnd);
void r_cnd_inform(r_obj* cnd, bool mufflable);
void r_cnd_warn(r_obj* cnd, bool mufflable);
void r_cnd_abort(r_obj* cnd, bool mufflable);

enum r_cnd_type {
  R_CND_TYPE_condition = 0,
  R_CND_TYPE_message = 1,
  R_CND_TYPE_warning = 2,
  R_CND_TYPE_error = 3,
  R_CND_TYPE_interrupt = 4
};

enum r_cnd_type r_cnd_type(r_obj* cnd);


#endif
