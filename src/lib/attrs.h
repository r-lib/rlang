#ifndef RLANG_ATTRS_H
#define RLANG_ATTRS_H

#include "sym.h"


static inline
sexp* r_attrib(sexp* x) {
  return ATTRIB(x);
}
static inline
sexp* r_poke_attrib(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}

sexp* r_attrib_get(sexp* x, sexp* tag);
sexp* r_attrib_push(sexp* x, sexp* tag, sexp* value);
sexp* r_attrib_set(sexp* x, sexp* tag, sexp* value);

static inline
void r_push_names(sexp* x, sexp* value) {
  r_attrib_push(x, R_NamesSymbol, value);
}

sexp* r_node_push_classes(sexp* x, const char** tags);
sexp* r_node_push_class(sexp* x, const char* tag);

void r_push_classes(sexp* x, const char** tags);
void r_push_class(sexp* x, const char* tag);

static inline
void r_poke_attribute(sexp* x, sexp* sym, sexp* value) {
  Rf_setAttrib(x, sym, value);
}
static inline
void r_poke_class(sexp* x, sexp* classes) {
  r_poke_attribute(x, r_class_sym, classes);
}

static inline
sexp* r_set_class(sexp* x, sexp* classes) {
  x = r_attrib_set(x, r_class_sym, classes);

  if (classes == r_null) {
    r_unmark_object(x);
  } else {
    r_mark_object(x);
  }

  return x;
}
static inline
sexp* r_class(sexp* x) {
  return r_attrib_get(x, r_class_sym);
}

static inline
sexp* r_names(sexp* x) {
  return r_attrib_get(x, r_names_sym);
}
static inline
void r_poke_names(sexp* x, sexp* nms) {
  r_poke_attribute(x, r_names_sym, nms);
}

bool r_has_name_at(sexp* x, r_ssize i);
bool r_is_named(sexp* x);



#endif
