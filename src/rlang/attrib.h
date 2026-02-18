// IWYU pragma: private; include "rlang.h"

#ifndef RLANG_ATTRIB_H
#define RLANG_ATTRIB_H

#include "rlang-types.h"
#include "globals.h"
#include "node.h"


// Polyfill for R < 4.5.0
#if R_VERSION < R_Version(4, 5, 0)
static inline
int ANY_ATTRIB(SEXP x) {
  return ATTRIB(x) != R_NilValue;
}
static inline
void CLEAR_ATTRIB(SEXP x) {
  SET_ATTRIB(x, R_NilValue);
  SET_OBJECT(x, 0);
  UNSET_S4_OBJECT(x);
}
#endif

// Polyfill for R < 4.6.0
#if R_VERSION < R_Version(4, 6, 0)
static inline
SEXP R_mapAttrib(SEXP x, SEXP (*FUN)(SEXP, SEXP, void *), void *data) {
  PROTECT_INDEX api;
  SEXP a = ATTRIB(x);
  SEXP val = NULL;

  PROTECT_WITH_INDEX(a, &api);
  while (a != R_NilValue) {
    SEXP tag = PROTECT(TAG(a));
    SEXP attr = PROTECT(CAR(a));
    val = FUN(tag, attr, data);
    UNPROTECT(2);
    if (val != NULL)
      break;
    REPROTECT(a = CDR(a), api);
  }
  UNPROTECT(1);
  return val;
}
#endif


static inline
bool r_attrib_has_any(r_obj* x) {
  return ANY_ATTRIB(x);
}

r_obj* r_attrib_collect(r_obj* x);

static inline
void r_attrib_zap(r_obj* x, r_obj* tag) {
  Rf_setAttrib(x, tag, r_null);
}
static inline
void r_attrib_zap_all(r_obj* x) {
  CLEAR_ATTRIB(x);
}

static inline
void r_attrib_poke_from(r_obj* to, r_obj* from) {
  SHALLOW_DUPLICATE_ATTRIB(to, from);
}


// Unlike Rf_getAttrib(), this never allocates. This also doesn't bump
// refcounts or namedness.
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