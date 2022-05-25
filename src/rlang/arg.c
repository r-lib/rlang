#include "rlang.h"

int (*r_arg_match)(r_obj* arg,
                   r_obj* values,
                   struct r_lazy error_arg,
                   struct r_lazy error_call);

void r_init_library_arg() {
  r_arg_match = (int (*)(r_obj*, r_obj*, struct r_lazy, struct r_lazy))
    r_peek_c_callable("rlang", "rlang_arg_match_2");
}
