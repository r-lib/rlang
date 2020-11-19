#include <rlang.h>
#include "utils.h"

// Defined in capture.c
sexp* rlang_capturearginfo(sexp* call, sexp* op, sexp* args, sexp* rho);
sexp* rlang_capturedots(sexp* call, sexp* op, sexp* args, sexp* rho);

sexp* rlang_ext_capturearginfo(sexp* args) {
  args = r_node_cdr(args);
  sexp* env = r_node_car(args); args = r_node_cdr(args);
  return rlang_capturearginfo(r_null, r_null, args, env);
}

sexp* rlang_ext_capturedots(sexp* args) {
  args = r_node_cdr(args);
  return rlang_capturedots(r_null, r_null, args, r_base_env);
}
