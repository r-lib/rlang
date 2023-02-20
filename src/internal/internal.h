#ifndef RLANG_INTERNAL_INTERNAL_H
#define RLANG_INTERNAL_INTERNAL_H

#include <rlang.h>

#include "arg.h"
#include "call.h"
#include "globals.h"
#include "quo.h"
#include "vec.h"

struct rlang_globals_syms {
  r_obj* c_null;
  r_obj* handlers;
  r_obj* tryCatch;
  r_obj* withCallingHandlers;
};

extern r_obj* rlang_zap;

extern r_obj* rlang_as_list_call;

extern r_obj* rlang_objs_keep;
extern r_obj* rlang_objs_trailing;

extern r_obj* fns_function;
extern r_obj* fns_quote;

void rlang_init_internal(r_obj* ns);
r_obj* rlang_ns_get(const char* name);

// From dots.c
r_obj* dots_values_node_impl(r_obj* frame_env,
                             r_obj* named,
                             r_obj* ignore_empty,
                             r_obj* preserve_empty,
                             r_obj* unquote_names,
                             r_obj* homonyms,
                             r_obj* check_assign,
                             bool splice);

static inline r_obj* rlang_dots(r_obj* env) {
  return dots_values_node_impl(env,
                               r_false,
                               rlang_objs_trailing,
                               r_true,
                               r_true,
                               rlang_objs_keep,
                               r_false,
                               true);
}

r_obj* ffi_replace_na(r_obj* x, r_obj* replacement);

r_obj* rlang_as_function(r_obj* x, r_obj* env);

extern struct rlang_globals_syms rlang_syms;

// From cnd.c
// Protects with the vmax stack
const char* obj_type_friendly(r_obj* x);


#endif
