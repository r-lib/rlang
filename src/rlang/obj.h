#ifndef RLANG_OBJ_H
#define RLANG_OBJ_H

#define r_missing_arg R_MissingArg


static inline
r_ssize r_length(r_obj* x) {
  return Rf_xlength(x);
}

static inline
enum r_type r_typeof(r_obj* x) {
  return (enum r_type) TYPEOF(x);
}

void _r_preserve(r_obj* x);
void _r_unpreserve(r_obj* x);

static r_unused
r_obj* _r_placeholder = NULL;

#define r_preserve(X)                           \
  (R_PreserveObject(_r_placeholder = X),        \
   (_r_preserve)(_r_placeholder),               \
   (void) NULL)

#define r_unpreserve(X)                         \
  (R_ReleaseObject(_r_placeholder = X),         \
   (_r_unpreserve)(_r_placeholder),             \
   (void) NULL)

static inline
void r_mark_shared(r_obj* x) {
  MARK_NOT_MUTABLE(x);
}
static inline
bool r_is_shared(r_obj* x) {
  return MAYBE_REFERENCED(x);
}

static inline
void _r_preserve_global(r_obj* x) {
  (_r_preserve)(x);
  r_mark_shared(x);
}

#define r_preserve_global(X)                    \
  (R_PreserveObject(_r_placeholder = X),        \
   (_r_preserve_global)(_r_placeholder),         \
   (void) NULL)

static inline
void r_mark_object(r_obj* x) {
  SET_OBJECT(x, 1);
}
static inline
void r_unmark_object(r_obj* x) {
  SET_OBJECT(x, 0);
}
static inline
bool r_is_object(r_obj* x) {
  return OBJECT(x);
}

static inline
bool r_inherits(r_obj* x, const char* tag) {
  return Rf_inherits(x, tag);
}

static inline
r_obj* r_copy(r_obj* x) {
  return Rf_duplicate(x);
}
static inline
r_obj* r_clone(r_obj* x) {
  return Rf_shallow_duplicate(x);
}
static inline
r_obj* r_clone_shared(r_obj* x) {
  return r_is_shared(x) ? r_clone(x) : x;
}

// These also clone names
r_obj* r_vec_clone(r_obj* x);
r_obj* r_vec_clone_shared(r_obj* x);

static inline
r_obj* r_poke_type(r_obj* x, enum r_type type) {
  SET_TYPEOF(x, type);
  return x;
}

static inline
r_obj* r_type_as_string(enum r_type type) {
  return Rf_type2str(type);
}
static inline
r_obj* r_type_as_character(enum r_type type) {
  r_obj* str = KEEP(r_type_as_string(type));
  r_obj* out = Rf_ScalarString(str);
  return FREE(1), out;
}
static inline
const char* r_type_as_c_string(enum r_type type) {
  return CHAR(Rf_type2str(type));
}

static inline
enum r_type r_c_str_as_r_type(const char* type) {
  return (enum r_type) Rf_str2type(type);
}
enum r_type r_chr_as_r_type(r_obj* type);

static inline
bool r_is_symbolic(r_obj* x) {
  return
    r_typeof(x) == LANGSXP ||
    r_typeof(x) == SYMSXP;
}

static inline
void r_obj_print(r_obj* x) {
  Rf_PrintValue(x);
}

static inline
bool r_is_identical(r_obj* x, r_obj* y) {
  // 16 corresponds to base::identical()'s defaults
  // Do we need less conservative versions?
  return R_compute_identical(x, y, 16);
}

r_obj* r_obj_address(r_obj* x);


extern r_obj* (*r_obj_encode_utf8)(r_obj* x);

r_obj* r_as_label(r_obj* x);


#endif
