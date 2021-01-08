#include "rlang.h"

sexp* (*r_quo_get_expr)(sexp* quo);
sexp* (*r_quo_set_expr)(sexp* quo, sexp* expr);
sexp* (*r_quo_get_env)(sexp* quo);
sexp* (*r_quo_set_env)(sexp* quo, sexp* env);
