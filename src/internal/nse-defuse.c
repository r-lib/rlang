#include <rlang.h>
#include "utils.h"

// Defined in capture.c
sexp* rlang_capturearginfo(sexp* call, sexp* op, sexp* args, sexp* rho);
sexp* rlang_capturedots(sexp* call, sexp* op, sexp* args, sexp* rho);

sexp* rlang_ext2_capturearginfo(sexp* call, sexp* op, sexp* args, sexp* env) {
  return rlang_capturearginfo(call, op, r_node_cdr(args), env);
}

sexp* rlang_ext2_capturedots(sexp* call, sexp* op, sexp* args, sexp* env) {
  return rlang_capturedots(call, op, r_node_cdr(args), env);
}
