#include "rlang.h"

r_obj* (*r_quo_get_expr)(r_obj* quo);
r_obj* (*r_quo_set_expr)(r_obj* quo, r_obj* expr);
r_obj* (*r_quo_get_env)(r_obj* quo);
r_obj* (*r_quo_set_env)(r_obj* quo, r_obj* env);

void r_init_library_quo() {
  r_quo_get_expr = (r_obj* (*)(r_obj*)) r_peek_c_callable("rlang", "rlang_quo_get_expr");
  r_quo_set_expr = (r_obj* (*)(r_obj*, r_obj*)) r_peek_c_callable("rlang", "rlang_quo_set_expr");
  r_quo_get_env = (r_obj* (*)(r_obj*)) r_peek_c_callable("rlang", "rlang_quo_get_env");
  r_quo_set_env = (r_obj* (*)(r_obj*, r_obj*)) r_peek_c_callable("rlang", "rlang_quo_set_env");
}
