#ifndef RLANG_STATE_H
#define RLANG_STATE_H


static inline
sexp* r_peek_option(const char* name) {
  return Rf_GetOption1(Rf_install(name));
}


#endif
