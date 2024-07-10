#include <rlang.h>

// Defined in capture.c
r_obj* rlang_capturearginfo(r_obj* call, r_obj* op, r_obj* args, r_obj* rho);
r_obj* rlang_capturedots(r_obj* call, r_obj* op, r_obj* args, r_obj* rho);

r_obj* ffi_capturearginfo(r_obj* args) {
  args = r_node_cdr(args);
  r_obj* env = r_node_car(args); args = r_node_cdr(args);
  return rlang_capturearginfo(r_null, r_null, args, env);
}

r_obj* ffi_capturedots(r_obj* args) {
  args = r_node_cdr(args);
  return rlang_capturedots(r_null, r_null, args, r_envs.base);
}
