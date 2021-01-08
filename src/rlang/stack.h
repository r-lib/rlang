#ifndef RLANG_STACK_H
#define RLANG_STACK_H


void r_on_exit(sexp* expr, sexp* frame);

sexp* r_current_frame();
sexp* r_sys_frame(int n, sexp* frame);
sexp* r_sys_call(int n, sexp* frame);

static inline void r_maybe_interrupt() {
  R_CheckUserInterrupt();
}


#endif
