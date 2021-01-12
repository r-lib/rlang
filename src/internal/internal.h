#ifndef RLANG_INTERNAL_INTERNAL_H
#define RLANG_INTERNAL_INTERNAL_H

#include "quo.h"


extern sexp* rlang_zap;

extern sexp* rlang_as_list_call;

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
                               r_false,
                               rlang_objs_trailing,
                               r_true,
                               r_true,
                               rlang_objs_keep,
                               r_false,
                               true);
}

sexp* rlang_replace_na(sexp* x, sexp* replacement);


#endif
