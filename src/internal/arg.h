#ifndef RLANG_INTERNAL_ARG_H
#define RLANG_INTERNAL_ARG_H


int arg_match(r_obj* arg,
              r_obj* values,
              struct r_lazy error_arg,
              struct r_lazy error_call,
              struct r_lazy call);

int arg_match_legacy(r_obj* arg,
                     r_obj* values,
                     r_obj* error_arg,
                     r_obj* error_call);


#endif
