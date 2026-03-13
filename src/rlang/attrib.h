// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ATTRIB_H
#define RLANG_ATTRIB_H

#include "rlang-types.h"
#include "globals.h"


static inline
bool r_attrib_has_any(r_obj* x) {
  return ANY_ATTRIB(x);
}

// Collect attributes into a fresh pairlist
r_obj* r_attrib_collect(r_obj* x);

typedef r_obj* (r_attrib_map_fn)(r_obj* tag, r_obj* value, void* data);

// Map a callback to each attribute of an object. Prefer this to collecting for
// performance-critical applications.
static inline
r_obj* r_attrib_map(r_obj* x, r_attrib_map_fn* fn, void* data) {
  return R_mapAttrib(x, fn, data);
}

static inline
void r_attrib_zap(r_obj* x, r_obj* tag) {
  Rf_setAttrib(x, tag, r_null);
}
static inline
void r_attrib_zap_all(r_obj* x) {
  CLEAR_ATTRIB(x);
}

static inline
void r_attrib_clone_from(r_obj* to, r_obj* from) {
  SHALLOW_DUPLICATE_ATTRIB(to, from);
}


// Unlike Rf_getAttrib(), this doesn't allocate, but in practice requires
// protection because rchk considers the return value to be a fresh pointer.
r_obj* r_attrib_get(r_obj* x, r_obj* tag);

static inline
r_obj* r_class(r_obj* x) {
  return r_attrib_get(x, r_syms.class_);
}

void r_attrib_poke_classes(r_obj* x, const char** classes, r_ssize n);

static inline
r_obj* r_dim(r_obj* x) {
  return r_attrib_get(x, r_syms.dim);
}

static inline
r_obj* r_dim_names(r_obj* x) {
  return r_attrib_get(x, r_syms.dim_names);
}

static inline
r_obj* r_names(r_obj* x) {
  return r_attrib_get(x, r_syms.names);
}

bool r_is_named(r_obj* x);


// Defined as macros so rchk can see that `X` protects `VALUE`
#define r_attrib_poke(X, SYM, VALUE) Rf_setAttrib(X, SYM, VALUE)
#define r_attrib_poke_class(X, VALUE) Rf_setAttrib(X, r_syms.class_, VALUE)
#define r_attrib_poke_dim(X, VALUE) Rf_setAttrib(X, r_syms.dim, VALUE)
#define r_attrib_poke_dim_names(X, VALUE) Rf_setAttrib(X, r_syms.dim_names, VALUE)
#define r_attrib_poke_names(X, VALUE) Rf_setAttrib(X, r_syms.names, VALUE)


#endif
