// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_STACK_H
#define RLANG_STACK_H

#include "rlang-types.h"

void r_on_exit(r_obj* expr, r_obj* frame);

r_obj* r_peek_frame(void);
r_obj* r_caller_env(r_obj* n);
r_obj* r_sys_frame(int n, r_obj* frame);
r_obj* r_sys_call(int n, r_obj* frame);

static inline
void r_yield_interrupt(void) {
  R_CheckUserInterrupt();
}


#endif
