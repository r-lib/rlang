#ifndef RLANG_SEXP_H
#define RLANG_SEXP_H


static inline r_ssize_t r_length(sexp* x) {
  return Rf_length(x);
}

static inline enum r_type r_typeof(sexp* x) {
  return TYPEOF(x);
}

#define r_mark_precious R_PreserveObject
#define r_unmark_precious R_ReleaseObject

static inline void r_mark_shared(sexp* x) {
  MARK_NOT_MUTABLE(x);
}
static inline bool r_is_shared(sexp* x) {
  return MAYBE_SHARED(x);
}

static inline void r_mark_object(sexp* x) {
  SET_OBJECT(x, 1);
}
static inline void r_unmark_object(sexp* x) {
  SET_OBJECT(x, 0);
}
static inline bool r_is_object(sexp* x) {
  return OBJECT(x);
}

static inline bool r_inherits(sexp* x, const char* tag) {
  return Rf_inherits(x, tag);
}

static inline void r_poke_attribute(sexp* x, sexp* sym, sexp* value) {
  Rf_setAttrib(x, sym, value);
}
static inline void r_poke_class(sexp* x, sexp* classes) {
  Rf_setAttrib(x, R_ClassSymbol, classes);
}

sexp* r_set_attribute(sexp* x, sexp* sym, sexp* attr);

static inline sexp* r_set_class(sexp* x, sexp* classes) {
  return r_set_attribute(x, R_ClassSymbol, classes);
}

static inline sexp* r_get_class(sexp* x) {
  return Rf_getAttrib(x, R_ClassSymbol);
}

// From attrs.c
sexp* r_get_attribute(sexp* x, sexp* tag);

static inline sexp* r_vec_names(sexp* x) {
  return r_get_attribute(x, R_NamesSymbol);
}

static inline void r_poke_names(sexp* x, sexp* nms) {
  Rf_setAttrib(x, R_NamesSymbol, nms);
}

bool r_has_name_at(sexp* x, r_ssize_t i);
bool r_is_named(sexp* x);

static inline sexp* r_missing_arg() {
  return R_MissingArg;
}
static inline bool r_is_missing(sexp* x) {
  return x == R_MissingArg;
}

static inline bool r_is_null(sexp* x) {
  return x == R_NilValue;
}

static inline sexp* r_duplicate(sexp* x, bool shallow) {
  if (shallow) {
    return Rf_shallow_duplicate(x);
  } else {
    return Rf_duplicate(x);
  }
}

static inline sexp* r_maybe_duplicate(sexp* x, bool shallow) {
  if (r_is_shared(x)) {
    return r_duplicate(x, shallow);
  } else {
    return x;
  }
}

static inline sexp* r_poke_type(sexp* x, enum r_type type) {
  SET_TYPEOF(x, type);
  return x;
}
static inline sexp* r_poke_str_type(sexp* x, const char* type) {
  SET_TYPEOF(x, Rf_str2type(type));
  return x;
}

static inline const char* r_type_c_string(enum r_type type) {
  return CHAR(Rf_type2str(type));
}

static inline bool r_is_symbolic(sexp* x) {
  return
    r_typeof(x) == LANGSXP ||
    r_typeof(x) == SYMSXP;
}

static inline void r_sxp_print(sexp* x) {
  Rf_PrintValue(x);
}

static inline bool r_is_identical(sexp* x, sexp* y) {
  // 16 corresponds to base::identical()'s defaults
  // Do we need less conservative versions?
  return R_compute_identical(x, y, 16);
}


#endif
