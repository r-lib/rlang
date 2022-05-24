#ifndef RLANG_ARG_H
#define RLANG_ARG_H


extern int (*r_arg_match)(r_obj* arg,
                          r_obj* values,
                          struct r_lazy error_arg,
                          struct r_lazy error_call);


#endif
