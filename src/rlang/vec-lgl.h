// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_VECTOR_LGL_H
#define RLANG_VECTOR_LGL_H

#include "rlang-types.h"


r_ssize r_lgl_sum(r_obj* x, bool na_true);
r_obj* r_lgl_which(r_obj* x, bool na_propagate);


#endif
