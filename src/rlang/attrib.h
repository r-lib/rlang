#ifndef RLANG_ATTRIB_H
#define RLANG_ATTRIB_H

#include "node.h"
#include "sym.h"


static inline
r_obj* r_attrib(r_obj* x) {
  return ATTRIB(x);
}
static inline
r_obj* r_poke_attrib(r_obj* x, r_obj* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}

// Unlike Rf_getAttrib(), this never allocates. This also doesn't bump
// refcounts or namedness.
static inline
r_obj* r_attrib_get(r_obj* x, r_obj* tag) {
  return r_pairlist_get(r_attrib(x), tag);
}
static inline
void r_attrib_poke(r_obj* x, r_obj* sym, r_obj* value) {
  Rf_setAttrib(x, sym, value);
}

r_obj* r_attrib_push(r_obj* x, r_obj* tag, r_obj* value);
r_obj* r_attrib_set(r_obj* x, r_obj* tag, r_obj* value);

static inline
r_obj* r_class(r_obj* x) {
  return r_attrib_get(x, r_syms.class_);
}
static inline
void r_attrib_poke_class(r_obj* x, r_obj* classes) {
  r_attrib_poke(x, r_syms.class_, classes);
}

void r_attrib_push_class(r_obj* x, const char* tag);
void r_attrib_push_classes(r_obj* x, const char** tags, r_ssize n);

static inline
r_obj* r_dim(r_obj* x) {
  return r_attrib_get(x, r_syms.dim);
}
static inline
void r_attrib_poke_dim(r_obj* x, r_obj* dim) {
  r_attrib_poke(x, r_syms.dim, dim);
}

static inline
r_obj* r_dim_names(r_obj* x) {
  return r_attrib_get(x, r_syms.dim_names);
}
static inline
void r_attrib_poke_dim_names(r_obj* x, r_obj* dim_names) {
  r_attrib_poke(x, r_syms.dim_names, dim_names);
}

static inline
r_obj* r_names(r_obj* x) {
  return r_attrib_get(x, r_syms.names);
}
static inline
void r_attrib_poke_names(r_obj* x, r_obj* nms) {
  r_attrib_poke(x, r_syms.names, nms);
}

bool r_is_named(r_obj* x);


#define r_attrib_poke(X, SYM, VALUE) Rf_setAttrib(X, SYM, VALUE)
#define r_attrib_poke_class(X, VALUE) Rf_setAttrib(X, r_syms.class_, VALUE)
#define r_attrib_poke_dim(X, VALUE) Rf_setAttrib(X, r_syms.dim, VALUE)
#define r_attrib_poke_dim_names(X, VALUE) Rf_setAttrib(X, r_syms.dim_names, VALUE)
#define r_attrib_poke_names(X, VALUE) Rf_setAttrib(X, r_syms.names, VALUE)


#endif
