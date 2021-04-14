#ifndef RLANG_GLOBALS_H
#define RLANG_GLOBALS_H


struct r_globals_classes {
  r_obj* data_frame;
  r_obj* tibble;
};

struct r_globals_chrs {
  r_obj* empty_string;
};

struct r_globals {
  r_obj* empty_lgl;
  r_obj* empty_int;
  r_obj* empty_dbl;
  r_obj* empty_cpl;
  r_obj* empty_raw;
  r_obj* empty_str;
  r_obj* empty_chr;
  r_obj* empty_list;

  int na_lgl;
  int na_int;
  double na_dbl;
  r_obj* na_str;
};

struct r_globals_syms {
  r_obj* class;
  r_obj* dots;
  r_obj* dot_environment;
  r_obj* dot_fn;
  r_obj* dot_x;
  r_obj* dot_y;
  r_obj* function;
  r_obj* missing;
  r_obj* names;
  r_obj* colon2;
  r_obj* colon3;
  r_obj* srcref;
  r_obj* row_names;
  r_obj* tilde;
  r_obj* unbound;
  r_obj* w;
  r_obj* x;
  r_obj* y;
  r_obj* z;
};

extern struct r_globals r_globals;
extern struct r_globals_chrs r_chrs;
extern struct r_globals_classes r_classes;
extern struct r_globals_syms r_syms;

extern r_obj* r_true;
extern r_obj* r_false;


#endif
