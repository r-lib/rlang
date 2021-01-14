#ifndef RLANG_SEXP_H
#define RLANG_SEXP_H

#define r_missing_arg R_MissingArg


static inline
r_ssize r_length(sexp* x) {
  return Rf_xlength(x);
}

static inline
enum r_type r_typeof(sexp* x) {
  return TYPEOF(x);
}

#define r_mark_precious R_PreserveObject
#define r_unmark_precious R_ReleaseObject

static inline
void r_mark_shared(sexp* x) {
  MARK_NOT_MUTABLE(x);
}
static inline
bool r_is_shared(sexp* x) {
  return MAYBE_REFERENCED(x);
}

static inline
void r_mark_object(sexp* x) {
  SET_OBJECT(x, 1);
}
static inline
void r_unmark_object(sexp* x) {
  SET_OBJECT(x, 0);
}
static inline
bool r_is_object(sexp* x) {
  return OBJECT(x);
}

static inline
bool r_inherits(sexp* x, const char* tag) {
  return Rf_inherits(x, tag);
}

static inline
sexp* r_copy(sexp* x) {
  return Rf_duplicate(x);
}
static inline
sexp* r_clone(sexp* x) {
  return Rf_shallow_duplicate(x);
}

static inline
sexp* r_poke_type(sexp* x, enum r_type type) {
  SET_TYPEOF(x, type);
  return x;
}

static inline
sexp* r_type_as_string(enum r_type type) {
  return Rf_type2str(type);
}
static inline
sexp* r_type_as_character(enum r_type type) {
  sexp* str = KEEP(r_type_as_string(type));
  sexp* out = Rf_ScalarString(str);
  return FREE(1), out;
}
static inline
const char* r_type_as_c_string(enum r_type type) {
  return CHAR(Rf_type2str(type));
}

static inline
bool r_is_symbolic(sexp* x) {
  return
    r_typeof(x) == LANGSXP ||
    r_typeof(x) == SYMSXP;
}

static inline
void r_sexp_print(sexp* x) {
  Rf_PrintValue(x);
}

static inline
bool r_is_identical(sexp* x, sexp* y) {
  // 16 corresponds to base::identical()'s defaults
  // Do we need less conservative versions?
  return R_compute_identical(x, y, 16);
}

static inline
sexp* r_sexp_address(sexp* x) {
  static char buf[1000];
  snprintf(buf, 1000, "%p", (void*) x);
  return Rf_mkChar(buf);
}

#endif
