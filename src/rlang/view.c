#include "rlang.h"

#include <R_ext/Altrep.h>

/*
Structure of a `view`:

Before materialization:
- `data1` is the original vector
- `data2` is a RAWSXP holding a `r_view_metadata`

After materialization:
- `data1` is `R_NilValue`
- `data2` is the materialized view

So `data1 == R_NilValue` is how we determine if we have materialized or not
*/

struct r_view_metadata {
  r_ssize start;
  r_ssize size;
};

// Initialised at load time
R_altrep_class_t r_lgl_view_class;
R_altrep_class_t r_int_view_class;
R_altrep_class_t r_dbl_view_class;
R_altrep_class_t r_cpl_view_class;
R_altrep_class_t r_raw_view_class;
R_altrep_class_t r_chr_view_class;
R_altrep_class_t r_list_view_class;

// -----------------------------------------------------------------------------

static inline r_obj*
r_view(R_altrep_class_t cls, r_obj* x, r_ssize start, r_ssize size) {
  if (r_attrib(x) != r_null) {
    r_stop_internal("`x` can't have any attributes.");
  }

  // We don't want it to have any chance of changing out from under us
  r_mark_shared(x);

  r_obj* metadata = r_alloc_raw(sizeof(struct r_view_metadata));
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);
  p_metadata->start = start;
  p_metadata->size = size;

  return R_new_altrep(cls, x, metadata);
}

static inline r_obj* r_lgl_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_lgl_view_class, x, start, size);
}
static inline r_obj* r_int_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_int_view_class, x, start, size);
}
static inline r_obj* r_dbl_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_dbl_view_class, x, start, size);
}
static inline r_obj* r_cpl_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_cpl_view_class, x, start, size);
}
static inline r_obj* r_raw_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_raw_view_class, x, start, size);
}
static inline r_obj* r_chr_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_chr_view_class, x, start, size);
}
static inline r_obj* r_list_view(r_obj* x, r_ssize start, r_ssize size) {
  return r_view(r_list_view_class, x, start, size);
}

// Up to the caller to verify that `start` and `size` are sized correctly.
// `start` is 0-indexed.
r_obj* r_vec_view(r_obj* x, r_ssize start, r_ssize size) {
  switch (r_typeof(x)) {
    case R_TYPE_logical:
      return r_lgl_view(x, start, size);
    case R_TYPE_integer:
      return r_int_view(x, start, size);
    case R_TYPE_double:
      return r_dbl_view(x, start, size);
    case R_TYPE_complex:
      return r_cpl_view(x, start, size);
    case R_TYPE_raw:
      return r_raw_view(x, start, size);
    case R_TYPE_character:
      return r_chr_view(x, start, size);
    case R_TYPE_list:
      return r_list_view(x, start, size);
    default:
      r_stop_internal("Type not implemented.");
  }
}

// -----------------------------------------------------------------------------

static inline bool r_is_lgl_view(r_obj* x) {
  return R_altrep_inherits(x, r_lgl_view_class);
}
static inline bool r_is_int_view(r_obj* x) {
  return R_altrep_inherits(x, r_int_view_class);
}
static inline bool r_is_dbl_view(r_obj* x) {
  return R_altrep_inherits(x, r_dbl_view_class);
}
static inline bool r_is_cpl_view(r_obj* x) {
  return R_altrep_inherits(x, r_cpl_view_class);
}
static inline bool r_is_raw_view(r_obj* x) {
  return R_altrep_inherits(x, r_raw_view_class);
}
static inline bool r_is_chr_view(r_obj* x) {
  return R_altrep_inherits(x, r_chr_view_class);
}
static inline bool r_is_list_view(r_obj* x) {
  return R_altrep_inherits(x, r_list_view_class);
}

bool r_is_view(r_obj* x) {
  switch (r_typeof(x)) {
    case R_TYPE_logical:
      return r_is_lgl_view(x);
    case R_TYPE_integer:
      return r_is_int_view(x);
    case R_TYPE_double:
      return r_is_dbl_view(x);
    case R_TYPE_complex:
      return r_is_cpl_view(x);
    case R_TYPE_raw:
      return r_is_raw_view(x);
    case R_TYPE_character:
      return r_is_chr_view(x);
    case R_TYPE_list:
      return r_is_list_view(x);
    default:
      return false;
  }
}

void r_check_view(r_obj* x) {
  if (r_is_view(x)) {
    return;
  }
  r_stop_internal("`x` must be an ALTREP view.");
}

// -----------------------------------------------------------------------------

#define R_VIEW_MATERIALIZE(ALLOC, CTYPE, BEGIN, GET_REGION)                  \
  r_obj* data = r_altrep_data1(x);                                           \
                                                                             \
  if (data == r_null) {                                                      \
    r_stop_internal(                                                         \
        "`x` has already been materialized, return `data2` directly rather " \
        "than calling this."                                                 \
    );                                                                       \
  }                                                                          \
                                                                             \
  r_obj* metadata = r_altrep_data2(x);                                       \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);                \
                                                                             \
  const r_ssize start = p_metadata->start;                                   \
  const r_ssize size = p_metadata->size;                                     \
                                                                             \
  r_obj* out = KEEP(ALLOC(size));                                            \
  CTYPE* v_out = BEGIN(out);                                                 \
                                                                             \
  /* Be friendly to ALTREP `data` too */                                     \
  GET_REGION(data, start, size, v_out);                                      \
                                                                             \
  /* Declare ourselves as materialized */                                    \
  R_set_altrep_data1(x, r_null);                                             \
  R_set_altrep_data2(x, out);                                                \
                                                                             \
  FREE(1);                                                                   \
  return out

#define R_VIEW_MATERIALIZE_BARRIER(ALLOC, CTYPE, CBEGIN, POKE)               \
  r_obj* data = r_altrep_data1(x);                                           \
                                                                             \
  if (data == r_null) {                                                      \
    r_stop_internal(                                                         \
        "`x` has already been materialized, return `data2` directly rather " \
        "than calling this."                                                 \
    );                                                                       \
  }                                                                          \
                                                                             \
  r_obj* metadata = r_altrep_data2(x);                                       \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);                \
                                                                             \
  const r_ssize start = p_metadata->start;                                   \
  const r_ssize size = p_metadata->size;                                     \
                                                                             \
  /* Read only pointer into original data, shifted to `start` */             \
  CTYPE const* v_data = CBEGIN(data) + start;                                \
                                                                             \
  r_obj* out = KEEP(ALLOC(size));                                            \
                                                                             \
  for (r_ssize i = 0; i < size; ++i) {                                       \
    r_obj* elt = v_data[i];                                                  \
    POKE(out, i, elt);                                                       \
  }                                                                          \
                                                                             \
  /* Declare ourselves as materialized */                                    \
  R_set_altrep_data1(x, r_null);                                             \
  R_set_altrep_data2(x, out);                                                \
                                                                             \
  FREE(1);                                                                   \
  return out

static r_obj* r_lgl_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(r_alloc_logical, int, r_lgl_begin, LOGICAL_GET_REGION);
}
static r_obj* r_int_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(r_alloc_integer, int, r_int_begin, INTEGER_GET_REGION);
}
static r_obj* r_dbl_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(r_alloc_double, double, r_dbl_begin, REAL_GET_REGION);
}
static r_obj* r_cpl_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(
      r_alloc_complex, r_complex, r_cpl_begin, COMPLEX_GET_REGION
  );
}
static r_obj* r_raw_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(r_alloc_raw, r_byte, r_raw_begin0, RAW_GET_REGION);
}
static r_obj* r_chr_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE_BARRIER(
      r_alloc_character, r_obj*, r_chr_cbegin, r_chr_poke
  );
}
static r_obj* r_list_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE_BARRIER(r_alloc_list, r_obj*, r_list_cbegin, r_list_poke);
}

r_obj* r_view_materialize(r_obj* x) {
  switch (r_typeof(x)) {
    case R_TYPE_logical:
      return r_lgl_view_materialize(x);
    case R_TYPE_integer:
      return r_int_view_materialize(x);
    case R_TYPE_double:
      return r_dbl_view_materialize(x);
    case R_TYPE_complex:
      return r_cpl_view_materialize(x);
    case R_TYPE_raw:
      return r_raw_view_materialize(x);
    case R_TYPE_character:
      return r_chr_view_materialize(x);
    case R_TYPE_list:
      return r_list_view_materialize(x);
    default:
      r_stop_internal("Type not implemented.");
  }
}

// -----------------------------------------------------------------------------

#define R_VIEW_DATAPTR_WRITABLE(MATERIALIZE, BEGIN)                \
  r_obj* out = NULL;                                               \
  r_obj* data = r_altrep_data1(x);                                 \
                                                                   \
  if (data != r_null) {                                            \
    /* We can't give out a writable pointer to `data`. */          \
    /* Materialize and give a writable pointer to that instead. */ \
    out = MATERIALIZE(x);                                          \
  } else {                                                         \
    /* Already materialized */                                     \
    out = r_altrep_data2(x);                                       \
  }                                                                \
                                                                   \
  return BEGIN(out);

static inline int* r_lgl_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_lgl_view_materialize, r_lgl_begin);
}
static inline int* r_int_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_int_view_materialize, r_int_begin);
}
static inline double* r_dbl_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_dbl_view_materialize, r_dbl_begin);
}
static inline r_complex* r_cpl_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_cpl_view_materialize, r_cpl_begin);
}
static inline r_byte* r_raw_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_raw_view_materialize, r_raw_begin0);
}
static inline void* r_chr_view_dataptr_writable(r_obj* x) {
  // R's internal usage of `STRING_PTR()` forces us to implement this,
  // but we should never call this function ourselves. `STRING_PTR()` is also
  // non-API, so we have to use `DATAPTR()` to get the writable pointer.
  R_VIEW_DATAPTR_WRITABLE(r_chr_view_materialize, DATAPTR);
}
// static inline void r_list_view_dataptr_writable(r_obj* x) {
//   // R does not use `VECTOR_PTR()` internally, and it even errors in
//   // `ALTVEC_DATAPTR_EX()` if you try and take a `writable` `DATAPTR()` on an
//   // ALTREP list, so we don't need this.
// }

#define R_VIEW_DATAPTR_READONLY(CBEGIN)                                \
  r_obj* data = r_altrep_data1(x);                                     \
                                                                       \
  if (data != r_null) {                                                \
    /* Provide a readonly view into the data at the right offset */    \
    r_obj* metadata = r_altrep_data2(x);                               \
    const struct r_view_metadata* p_metadata = r_raw_cbegin(metadata); \
    return CBEGIN(data) + p_metadata->start;                           \
  } else {                                                             \
    /* Provide a readonly view into the materialized data */           \
    return CBEGIN(r_altrep_data2(x));                                  \
  }

static inline int const* r_lgl_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_lgl_cbegin);
}
static inline int const* r_int_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_int_cbegin);
}
static inline double const* r_dbl_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_dbl_cbegin);
}
static inline r_complex const* r_cpl_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_cpl_cbegin);
}
static inline r_byte const* r_raw_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_raw_cbegin0);
}
static inline r_obj* const* r_chr_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_chr_cbegin);
}
static inline r_obj* const* r_list_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_list_cbegin);
}

#define R_VIEW_DATAPTR(WRITABLE, READONLY) \
  if (writable) {                          \
    return (void*) WRITABLE(x);            \
  } else {                                 \
    /* Caller promises not to mutate it */ \
    return (void*) READONLY(x);            \
  }

#define R_VIEW_DATAPTR_BARRIER(READONLY)            \
  if (writable) {                                   \
    /* `ALTVEC_DATAPTR_EX()` should have errored */ \
    r_stop_unreachable();                           \
  } else {                                          \
    /* Caller promises not to mutate it */          \
    return (void*) READONLY(x);                     \
  }

static void* r_lgl_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR(r_lgl_view_dataptr_writable, r_lgl_view_dataptr_readonly);
}
static void* r_int_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR(r_int_view_dataptr_writable, r_int_view_dataptr_readonly);
}
static void* r_dbl_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR(r_dbl_view_dataptr_writable, r_dbl_view_dataptr_readonly);
}
static void* r_cpl_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR(r_cpl_view_dataptr_writable, r_cpl_view_dataptr_readonly);
}
static void* r_raw_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR(r_raw_view_dataptr_writable, r_raw_view_dataptr_readonly);
}
static void* r_chr_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR(r_chr_view_dataptr_writable, r_chr_view_dataptr_readonly);
}
static void* r_list_view_dataptr(r_obj* x, Rboolean writable) {
  R_VIEW_DATAPTR_BARRIER(r_list_view_dataptr_readonly);
}

// We can always provide a readonly view
static const void* r_lgl_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_lgl_view_dataptr_readonly(x);
}
static const void* r_int_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_int_view_dataptr_readonly(x);
}
static const void* r_dbl_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_dbl_view_dataptr_readonly(x);
}
static const void* r_cpl_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_cpl_view_dataptr_readonly(x);
}
static const void* r_raw_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_raw_view_dataptr_readonly(x);
}
static const void* r_chr_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_chr_view_dataptr_readonly(x);
}
static const void* r_list_view_dataptr_or_null(r_obj* x) {
  return (const void*) r_list_view_dataptr_readonly(x);
}

// -----------------------------------------------------------------------------

static r_ssize r_view_length(r_obj* x) {
  r_obj* data = r_altrep_data1(x);

  if (data != r_null) {
    // Pull from metadata
    r_obj* metadata = r_altrep_data2(x);
    const struct r_view_metadata* p_metadata = r_raw_cbegin(metadata);
    return p_metadata->size;
  } else {
    // Pull from materialized object
    return Rf_xlength(r_altrep_data2(x));
  }
}

// -----------------------------------------------------------------------------

static inline Rboolean r_view_inspect(
    const char* name,
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  Rprintf(
      "%s (materialized=%s)\n", name, r_altrep_data1(x) == r_null ? "T" : "F"
  );
  return TRUE;
}

static Rboolean r_lgl_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect(
      "altrep_logical_view", x, pre, deep, pvec, inspect_subtree
  );
}
static Rboolean r_int_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect(
      "altrep_integer_view", x, pre, deep, pvec, inspect_subtree
  );
}
static Rboolean r_dbl_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect(
      "altrep_double_view", x, pre, deep, pvec, inspect_subtree
  );
}
static Rboolean r_cpl_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect(
      "altrep_complex_view", x, pre, deep, pvec, inspect_subtree
  );
}
static Rboolean r_raw_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect("altrep_raw_view", x, pre, deep, pvec, inspect_subtree);
}
static Rboolean r_chr_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect(
      "altrep_character_view", x, pre, deep, pvec, inspect_subtree
  );
}
static Rboolean r_list_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect(
      "altrep_list_view", x, pre, deep, pvec, inspect_subtree
  );
}

// -----------------------------------------------------------------------------

static r_obj* r_view_serialized_state(r_obj* x) {
  // Falls back to materializing the full object and serializing that,
  // no ALTREP used in the serialization. Particularly important to ensure
  // we can iterate on the internal structure without worrying about loading
  // old serialized ALTREP objects.
  return NULL;
}

// -----------------------------------------------------------------------------

#define R_VIEW_ELT(ELT)                                                \
  r_obj* data = r_altrep_data1(x);                                     \
                                                                       \
  if (data != r_null) {                                                \
    /* Element comes from original data */                             \
    r_obj* metadata = r_altrep_data2(x);                               \
    const struct r_view_metadata* p_metadata = r_raw_cbegin(metadata); \
    return ELT(data, p_metadata->start + i);                           \
  } else {                                                             \
    /* Element comes from materialized data */                         \
    return ELT(r_altrep_data2(x), i);                                  \
  }

static int r_lgl_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(LOGICAL_ELT);
}
static int r_int_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(INTEGER_ELT);
}
static double r_dbl_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(REAL_ELT);
}
static r_complex r_cpl_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(COMPLEX_ELT);
}
static r_byte r_raw_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(RAW_ELT);
}
static r_obj* r_chr_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(STRING_ELT);
}
static r_obj* r_list_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(VECTOR_ELT);
}

// -----------------------------------------------------------------------------

#define R_VIEW_SET_ELT(MATERIALIZE, POKE)                                  \
  r_obj* data = r_altrep_data1(x);                                         \
                                                                           \
  if (data != r_null) {                                                    \
    /* Materialize so we can set the element. */                           \
    /* Only protect `elt` when we materialize, for performance. */         \
    /* (although gc is disabled here anyways by `ALT<TYPE>_SET_ELT()`). */ \
    KEEP(elt);                                                             \
    data = MATERIALIZE(x);                                                 \
    POKE(data, i, elt);                                                    \
    FREE(1);                                                               \
  } else {                                                                 \
    /* Already materialized */                                             \
    data = r_altrep_data2(x);                                              \
    POKE(data, i, elt);                                                    \
  }

static void r_chr_view_set_elt(r_obj* x, r_ssize i, r_obj* elt) {
  R_VIEW_SET_ELT(r_chr_view_materialize, r_chr_poke);
}
static void r_list_view_set_elt(r_obj* x, r_ssize i, r_obj* elt) {
  R_VIEW_SET_ELT(r_list_view_materialize, r_list_poke);
}

// -----------------------------------------------------------------------------

// Purposefully not implemented
//
// R_set_altvec_Extract_subset_method
// This falls back to a default implementation that uses the `Elt` method,
// which we think is good enough (though it is slower)
//
// R_set_alttype_Get_region_method
// This first tries Dataptr_or_null, which we have a very efficient method
// for. It never returns `NULL` since we can always return a readonly pointer.
// No ALTREP `Get_region` method possible for character vectors or lists.

static void r_init_library_lgl_view(DllInfo* dll, const char* package) {
  r_lgl_view_class = R_make_altlogical_class("logical_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_lgl_view_class, r_lgl_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_lgl_view_class, r_lgl_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_lgl_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_lgl_view_class, r_lgl_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_lgl_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altlogical_Elt_method(r_lgl_view_class, r_lgl_view_elt);
}

static void r_init_library_int_view(DllInfo* dll, const char* package) {
  r_int_view_class = R_make_altinteger_class("integer_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_int_view_class, r_int_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_int_view_class, r_int_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_int_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_int_view_class, r_int_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_int_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altinteger_Elt_method(r_int_view_class, r_int_view_elt);
}

static void r_init_library_dbl_view(DllInfo* dll, const char* package) {
  r_dbl_view_class = R_make_altreal_class("double_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_dbl_view_class, r_dbl_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_dbl_view_class, r_dbl_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_dbl_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_dbl_view_class, r_dbl_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_dbl_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altreal_Elt_method(r_dbl_view_class, r_dbl_view_elt);
}

static void r_init_library_cpl_view(DllInfo* dll, const char* package) {
  r_cpl_view_class = R_make_altcomplex_class("complex_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_cpl_view_class, r_cpl_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_cpl_view_class, r_cpl_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_cpl_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_cpl_view_class, r_cpl_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_cpl_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altcomplex_Elt_method(r_cpl_view_class, r_cpl_view_elt);
}

static void r_init_library_raw_view(DllInfo* dll, const char* package) {
  r_raw_view_class = R_make_altraw_class("raw_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_raw_view_class, r_raw_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_raw_view_class, r_raw_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_raw_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_raw_view_class, r_raw_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_raw_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altraw_Elt_method(r_raw_view_class, r_raw_view_elt);
}

static void r_init_library_chr_view(DllInfo* dll, const char* package) {
  r_chr_view_class = R_make_altstring_class("character_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_chr_view_class, r_chr_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_chr_view_class, r_chr_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_chr_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_chr_view_class, r_chr_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_chr_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altstring_Elt_method(r_chr_view_class, r_chr_view_elt);
  R_set_altstring_Set_elt_method(r_chr_view_class, r_chr_view_set_elt);
}

static void r_init_library_list_view(DllInfo* dll, const char* package) {
  r_list_view_class = R_make_altlist_class("list_view", package, dll);

  // ALTVEC
  R_set_altvec_Dataptr_method(r_list_view_class, r_list_view_dataptr);
  R_set_altvec_Dataptr_or_null_method(
      r_list_view_class, r_list_view_dataptr_or_null
  );

  // ALTREP
  R_set_altrep_Length_method(r_list_view_class, r_view_length);
  R_set_altrep_Inspect_method(r_list_view_class, r_list_view_inspect);
  R_set_altrep_Serialized_state_method(
      r_list_view_class, r_view_serialized_state
  );

  // ALTTYPE
  R_set_altlist_Elt_method(r_list_view_class, r_list_view_elt);
  R_set_altlist_Set_elt_method(r_list_view_class, r_list_view_set_elt);
}

void r_init_library_view(DllInfo* dll, const char* package) {
  r_init_library_lgl_view(dll, package);
  r_init_library_int_view(dll, package);
  r_init_library_dbl_view(dll, package);
  r_init_library_cpl_view(dll, package);
  r_init_library_raw_view(dll, package);
  r_init_library_chr_view(dll, package);
  r_init_library_list_view(dll, package);
}
