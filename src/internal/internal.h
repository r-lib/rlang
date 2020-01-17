#ifndef RLANG_INTERNAL_INTERNAL_H
#define RLANG_INTERNAL_INTERNAL_H

#include "quo.h"


extern sexp* rlang_zap;

extern sexp* as_list_call;
extern sexp* as_list_s4_call;

extern sexp* rlang_objs_keep;
extern sexp* rlang_objs_trailing;

extern sexp* fns_function;
extern sexp* fns_quote;

void rlang_init_internal(sexp* ns);
sexp* rlang_ns_get(const char* name);

// From dots.c
sexp* dots_values_node_impl(sexp* frame_env,
                            sexp* named,
                            sexp* ignore_empty,
                            sexp* preserve_empty,
                            sexp* unquote_names,
                            sexp* homonyms,
                            sexp* check_assign,
                            bool splice);

static inline sexp* rlang_dots(sexp* env) {
  return dots_values_node_impl(env,
                               r_shared_false,
                               rlang_objs_trailing,
                               r_shared_true,
                               r_shared_true,
                               rlang_objs_keep,
                               r_shared_false,
                               true);
}


#endif
