#include "lib/rlang.h"


sexp* rlanglibtest_r_quo_get_expr(sexp* quo) {
  return r_quo_get_expr(quo);
}
sexp* rlanglibtest_r_quo_set_expr(sexp* quo, sexp* expr) {
  return r_quo_set_expr(quo, expr);
}
sexp* rlanglibtest_r_quo_get_env(sexp* quo) {
  return r_quo_get_env(quo);
}
sexp* rlanglibtest_r_quo_set_env(sexp* quo, sexp* env) {
  return r_quo_set_env(quo, env);
}
