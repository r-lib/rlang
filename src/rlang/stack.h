#ifndef RLANG_STACK_H
#define RLANG_STACK_H


void r_on_exit(r_obj* expr, r_obj* frame);

r_obj* r_peek_frame();
r_obj* r_caller_env(r_obj* n);
r_obj* r_sys_frame(int n, r_obj* frame);
r_obj* r_sys_call(int n, r_obj* frame);

static inline
void r_yield_interrupt() {
  R_CheckUserInterrupt();
}


#endif
