#ifndef RLANG_FORMULA_H
#define RLANG_FORMULA_H


bool r_is_formula(r_obj* x, int scoped, int lhs);

r_obj* r_f_rhs(r_obj* f);
r_obj* r_f_lhs(r_obj* f);
r_obj* r_f_env(r_obj* f);
bool r_f_has_env(r_obj* f);


#endif
