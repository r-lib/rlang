#include "rlang.h"

int (*r_arg_match)(r_obj* arg, r_obj* values, r_obj* error_arg, r_obj* error_call);

void r_init_library_arg() {
  r_arg_match = (int (*)(r_obj*, r_obj*, r_obj*, r_obj*)) r_peek_c_callable("rlang", "rlang_arg_match");
}
