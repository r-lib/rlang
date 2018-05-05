#ifndef RLANG_ATTRS_H
#define RLANG_ATTRS_H

#include "sym.h"


static inline sexp* r_get_attributes(sexp* x) {
  return ATTRIB(x);
}
static inline void r_poke_attributes(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
}

sexp* r_get_attribute(sexp* x, sexp* tag);
sexp* r_push_attribute(sexp* x, sexp* tag, sexp* value);
sexp* r_set_attribute(sexp* x, sexp* tag, sexp* value);

static inline void r_push_names(sexp* x, sexp* value) {
  r_push_attribute(x, R_NamesSymbol, value);
}

sexp* r_node_push_classes(sexp* x, const char** tags);
sexp* r_node_push_class(sexp* x, const char* tag);

void r_push_classes(sexp* x, const char** tags);
void r_push_class(sexp* x, const char* tag);

static inline void r_poke_attribute(sexp* x, sexp* sym, sexp* value) {
  Rf_setAttrib(x, sym, value);
}
static inline void r_poke_class(sexp* x, sexp* classes) {
  r_poke_attribute(x, r_class_sym, classes);
}

static inline sexp* r_set_class(sexp* x, sexp* classes) {
  return r_set_attribute(x, r_class_sym, classes);
}
static inline sexp* r_get_class(sexp* x) {
  return r_get_attribute(x, r_class_sym);
}

static inline sexp* r_vec_names(sexp* x) {
  return r_get_attribute(x, r_names_sym);
}
static inline void r_poke_names(sexp* x, sexp* nms) {
  r_poke_attribute(x, r_names_sym, nms);
}

bool r_has_name_at(sexp* x, r_ssize_t i);
bool r_is_named(sexp* x);



#endif
