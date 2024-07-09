// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ARG_H
#define RLANG_ARG_H

#include "rlang-types.h"

extern int (*r_arg_match)(r_obj* arg,
                          r_obj* values,
                          struct r_lazy error_arg,
                          struct r_lazy error_call);


#endif
