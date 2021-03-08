#ifndef RLANG_GLOBALS_H
#define RLANG_GLOBALS_H


struct r_globals_classes {
  sexp* data_frame;
  sexp* tibble;
};

struct r_globals_syms {
  sexp* class;
  sexp* dots;
  sexp* dot_environment;
  sexp* dot_fn;
  sexp* dot_x;
  sexp* dot_y;
  sexp* function;
  sexp* missing;
  sexp* names;
  sexp* colon2;
  sexp* colon3;
  sexp* srcref;
  sexp* row_names;
  sexp* tilde;
  sexp* unbound;
  sexp* w;
  sexp* x;
  sexp* y;
  sexp* z;
};

extern struct r_globals_classes r_classes;
extern struct r_globals_syms r_syms;


#endif
