#include "rlang.h"

#define PRECIOUS_DICT_INIT_SIZE 256

static
struct r_dict precious_dict = { 0 };


void r_preserve(sexp* x) {
  r_dict_put(&precious_dict, x, r_null);
}
void r_unpreserve(sexp* x) {
  if (!r_dict_del(&precious_dict, x)) {
    r_abort("Can't unpreserve `x` because it was not being preserved.");
  }
}


void r_init_library_sexp(sexp* ns) {
  precious_dict = r_new_dict(PRECIOUS_DICT_INIT_SIZE);
  KEEP(precious_dict.shelter);
  r_env_poke(ns, r_sym(".__rlang_lib_precious_dict__."), precious_dict.shelter);
  FREE(1);
}
