#include "lib/rlang.h"


extern sexp* rlanglibtest_r_quo_get_expr(sexp*);
extern sexp* rlanglibtest_r_quo_set_expr(sexp*, sexp*);
extern sexp* rlanglibtest_r_quo_get_env(sexp*);
extern sexp* rlanglibtest_r_quo_set_env(sexp*, sexp*);


sexp* rlanglibtest_library_load() {
  r_init_library();
  return r_null;
}

static const r_callable r_callables[] = {
  {"rlanglibtest_library_load",   (r_fn_ptr) &rlanglibtest_library_load, 0},
  {"rlanglibtest_r_quo_get_expr", (r_fn_ptr) &rlanglibtest_r_quo_get_expr, 1},
  {"rlanglibtest_r_quo_set_expr", (r_fn_ptr) &rlanglibtest_r_quo_set_expr, 2},
  {"rlanglibtest_r_quo_get_env",  (r_fn_ptr) &rlanglibtest_r_quo_get_env, 1},
  {"rlanglibtest_r_quo_set_env",  (r_fn_ptr) &rlanglibtest_r_quo_set_env, 2},
  {NULL, NULL, 0}
};

void R_init_rlanglibtest(r_dll_info* dll) {
  r_register_r_callables(dll, r_callables, NULL);
}
