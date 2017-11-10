#ifndef RLANG_UTILS_H
#define RLANG_UTILS_H

#include <stdbool.h>


bool is_lazy_load(SEXP x);
SEXP make_formula1(SEXP rhs, SEXP env);
SEXP rlang_obj(const char* name);
SEXP base_obj(const char* name);
int is_symbolic(SEXP x);
int is_true(SEXP x);
bool is_character(SEXP x);
SEXP names(SEXP x);
bool has_name_at(SEXP x, r_size_t i);
bool is_str_empty(SEXP str);
bool is_object(SEXP x);
SEXP set_names(SEXP x, SEXP nms);
const char* kind_c_str(SEXPTYPE kind);
bool is_empty(SEXP x);


#endif
