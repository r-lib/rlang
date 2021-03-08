#ifndef RLANG_ATTRIB_H
#define RLANG_ATTRIB_H

#include "node.h"
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

// Unlike Rf_getAttrib(), this never allocates. This also doesn't bump
// refcounts or namedness.
static inline
sexp* r_attrib_get(sexp* x, sexp* tag) {
  return r_pairlist_get(r_attrib(x), tag);
}
static inline
void r_attrib_poke(sexp* x, sexp* sym, sexp* value) {
  Rf_setAttrib(x, sym, value);
}

sexp* r_attrib_push(sexp* x, sexp* tag, sexp* value);
sexp* r_attrib_set(sexp* x, sexp* tag, sexp* value);

static inline
sexp* r_class(sexp* x) {
  return r_attrib_get(x, r_syms.class);
}
static inline
void r_attrib_poke_class(sexp* x, sexp* classes) {
  r_attrib_poke(x, r_syms.class, classes);
}

void r_attrib_push_class(sexp* x, const char* tag);
void r_attrib_push_classes(sexp* x, const char** tags, r_ssize n);

static inline
sexp* r_names(sexp* x) {
  return r_attrib_get(x, r_syms.names);
}
static inline
void r_attrib_poke_names(sexp* x, sexp* nms) {
  r_attrib_poke(x, r_syms.names, nms);
}

bool r_is_named(sexp* x);


#endif
