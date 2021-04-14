#include "lib/rlang.h"


r_obj* rlanglibtest_r_quo_get_expr(r_obj* quo) {
  return r_quo_get_expr(quo);
}
r_obj* rlanglibtest_r_quo_set_expr(r_obj* quo, r_obj* expr) {
  return r_quo_set_expr(quo, expr);
}
r_obj* rlanglibtest_r_quo_get_env(r_obj* quo) {
  return r_quo_get_env(quo);
}
r_obj* rlanglibtest_r_quo_set_env(r_obj* quo, r_obj* env) {
  return r_quo_set_env(quo, env);
}
