#include <rlang.h>
#include "internal.h"


sexp* rlang_constants_env;

sexp* rlang_constants_get(const char* name) {
  return r_env_get(rlang_constants_env, r_sym(name));
}


void rlang_init_eval_tidy();

void rlang_init_internal() {
  // Should be first
  rlang_constants_env = rlang_ns_get("c_constants_env");

  rlang_init_eval_tidy();
}
