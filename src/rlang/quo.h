#ifndef RLANG_QUO_H
#define RLANG_QUO_H


extern r_obj* (*r_quo_get_expr)(r_obj* quo);
extern r_obj* (*r_quo_set_expr)(r_obj* quo, r_obj* expr);
extern r_obj* (*r_quo_get_env)(r_obj* quo);
extern r_obj* (*r_quo_set_env)(r_obj* quo, r_obj* env);


#endif
