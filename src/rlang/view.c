#include "rlang.h"

#include <R_ext/Altrep.h>

/*
Structure of a `view`:
- `data1` is:
  - the original vector, before materialization
  - the materialized vector, after materialization
- `data2` is a RAWSXP holding a `r_view_metadata`
*/

struct r_view_metadata {
  // Whether or not the ALTREP view has been materialized.
  bool materialized;

  // The offset into the original data to start at.
  r_ssize start;

  // The size of the view.
  r_ssize size;

  // A read only pointer into the data, to save indirection costs. We typically
  // set this upon view creation, unless `x` is ALTREP, in which case we delay
  // setting it until the first `DATAPTR_RO()` request, to be friendly to
  // ALTREP types that we wrap. After materialization, it is always set.
  const void* v_data_read;

  // A write only pointer into the data, to save indirection costs.
  // Always `NULL` before materialization, and it set at materialization time.
  // Never set for lists or character vectors, as write pointers are unsafe.
  void* v_data_write;
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

#define R_VIEW(CLS, CBEGIN)                                               \
  if (r_attrib(x) != r_null) {                                            \
    r_stop_internal("`x` can't have any attributes.");                    \
  }                                                                       \
                                                                          \
  /* We don't want it to have any chance of changing out from under us */ \
  r_mark_shared(x);                                                       \
                                                                          \
  r_obj* metadata = r_alloc_raw(sizeof(struct r_view_metadata));          \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);             \
  p_metadata->materialized = false;                                       \
  p_metadata->start = start;                                              \
  p_metadata->size = size;                                                \
  p_metadata->v_data_read = r_is_altrep(x) ? NULL : CBEGIN(x) + start;    \
  p_metadata->v_data_write = NULL;                                        \
                                                                          \
  return R_new_altrep(CLS, x, metadata)

static inline r_obj* r_lgl_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_lgl_view_class, r_lgl_cbegin);
}
static inline r_obj* r_int_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_int_view_class, r_int_cbegin);
}
static inline r_obj* r_dbl_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_dbl_view_class, r_dbl_cbegin);
}
static inline r_obj* r_cpl_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_cpl_view_class, r_cpl_cbegin);
}
static inline r_obj* r_raw_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_raw_view_class, r_raw_cbegin);
}
static inline r_obj* r_chr_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_chr_view_class, r_chr_cbegin);
}
static inline r_obj* r_list_view(r_obj* x, r_ssize start, r_ssize size) {
  R_VIEW(r_list_view_class, r_list_cbegin);
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

#define R_VIEW_MATERIALIZE(ALLOC, CTYPE, CBEGIN, BEGIN, GET_REGION)          \
  r_obj* metadata = r_altrep_data2(x);                                       \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);                \
                                                                             \
  if (p_metadata->materialized) {                                            \
    r_stop_internal(                                                         \
        "`x` has already been materialized, return `data1` directly rather " \
        "than calling this."                                                 \
    );                                                                       \
  }                                                                          \
                                                                             \
  r_obj* data = r_altrep_data1(x);                                           \
                                                                             \
  const r_ssize start = p_metadata->start;                                   \
  const r_ssize size = p_metadata->size;                                     \
                                                                             \
  r_obj* out = KEEP(ALLOC(size));                                            \
                                                                             \
  /* Go ahead and take dataptrs, we know this isn't ALTREP */                \
  CTYPE const* v_out_read = CBEGIN(out);                                     \
  CTYPE* v_out_write = BEGIN(out);                                           \
                                                                             \
  /* Materialize, but be friendly to ALTREP `data` too */                    \
  GET_REGION(data, start, size, v_out_write);                                \
                                                                             \
  /* Declare ourselves as materialized */                                    \
  p_metadata->materialized = true;                                           \
  p_metadata->v_data_read = v_out_read;                                      \
  p_metadata->v_data_write = v_out_write;                                    \
  R_set_altrep_data1(x, out);                                                \
                                                                             \
  FREE(1);                                                                   \
  return out

#define R_VIEW_MATERIALIZE_BARRIER(ALLOC, CTYPE, CBEGIN, POKE)               \
  r_obj* metadata = r_altrep_data2(x);                                       \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);                \
                                                                             \
  if (p_metadata->materialized) {                                            \
    r_stop_internal(                                                         \
        "`x` has already been materialized, return `data1` directly rather " \
        "than calling this."                                                 \
    );                                                                       \
  }                                                                          \
                                                                             \
  r_obj* data = r_altrep_data1(x);                                           \
                                                                             \
  const r_ssize start = p_metadata->start;                                   \
  const r_ssize size = p_metadata->size;                                     \
                                                                             \
  /* Read only pointer into original data, shifted to `start` */             \
  CTYPE const* v_data = CBEGIN(data) + start;                                \
                                                                             \
  r_obj* out = KEEP(ALLOC(size));                                            \
                                                                             \
  /* Go ahead and take readonly dataptr, we know this isn't ALTREP. */       \
  /* Never take writable dataptr for character vectors and lists. */         \
  CTYPE const* v_out_read = CBEGIN(out);                                     \
                                                                             \
  for (r_ssize i = 0; i < size; ++i) {                                       \
    r_obj* elt = v_data[i];                                                  \
    POKE(out, i, elt);                                                       \
  }                                                                          \
                                                                             \
  /* Declare ourselves as materialized */                                    \
  p_metadata->materialized = true;                                           \
  p_metadata->v_data_read = v_out_read;                                      \
  R_set_altrep_data1(x, out);                                                \
                                                                             \
  FREE(1);                                                                   \
  return out

static r_obj* r_lgl_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(
      r_alloc_logical, int, r_lgl_cbegin, r_lgl_begin, LOGICAL_GET_REGION
  );
}
static r_obj* r_int_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(
      r_alloc_integer, int, r_int_cbegin, r_int_begin, INTEGER_GET_REGION
  );
}
static r_obj* r_dbl_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(
      r_alloc_double, double, r_dbl_cbegin, r_dbl_begin, REAL_GET_REGION
  );
}
static r_obj* r_cpl_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(
      r_alloc_complex, r_complex, r_cpl_cbegin, r_cpl_begin, COMPLEX_GET_REGION
  );
}
static r_obj* r_raw_view_materialize(r_obj* x) {
  R_VIEW_MATERIALIZE(
      r_alloc_raw, r_byte, r_raw_cbegin0, r_raw_begin0, RAW_GET_REGION
  );
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

bool r_view_is_materialized(r_obj* x) {
  r_obj* metadata = r_altrep_data2(x);
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);
  return p_metadata->materialized;
}

// -----------------------------------------------------------------------------

#define R_VIEW_DATAPTR_WRITABLE(MATERIALIZE, CTYPE)           \
  r_obj* metadata = r_altrep_data2(x);                        \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata); \
                                                              \
  if (!p_metadata->materialized) {                            \
    /* This sets `p_metadata->v_data_write` */                \
    MATERIALIZE(x);                                           \
  }                                                           \
                                                              \
  return (CTYPE*) p_metadata->v_data_write

static inline int* r_lgl_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_lgl_view_materialize, int);
}
static inline int* r_int_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_int_view_materialize, int);
}
static inline double* r_dbl_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_dbl_view_materialize, double);
}
static inline r_complex* r_cpl_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_cpl_view_materialize, r_complex);
}
static inline r_byte* r_raw_view_dataptr_writable(r_obj* x) {
  R_VIEW_DATAPTR_WRITABLE(r_raw_view_materialize, r_byte);
}
static inline void* r_chr_view_dataptr_writable(r_obj* x) {
  // R's internal usage of `STRING_PTR()` forces us to implement this,
  // but we should never call this function ourselves. `STRING_PTR()` is also
  // non-API, so we have to use `DATAPTR()` to get the writable pointer.
  r_obj* metadata = r_altrep_data2(x);
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);

  if (!p_metadata->materialized) {
    /* This does not set `p_metadata->v_data_write` because that isn't safe, */
    /* but it does set `data1` to the materialized data */
    r_chr_view_materialize(x);
  }

  r_obj* data = r_altrep_data1(x);

  return DATAPTR(data);
}
// static inline void r_list_view_dataptr_writable(r_obj* x) {
//   // R does not use `VECTOR_PTR()` internally, and it even errors in
//   // `ALTVEC_DATAPTR_EX()` if you try and take a `writable` `DATAPTR()` on an
//   // ALTREP list, so we don't need this.
// }

#define R_VIEW_DATAPTR_READONLY(CTYPE, CBEGIN)                        \
  r_obj* metadata = r_altrep_data2(x);                                \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);         \
                                                                      \
  if (p_metadata->v_data_read != NULL) {                              \
    /* This is the typical case. Only unset if the original object */ \
    /* was ALTREP and we haven't requested `DATAPTR_RO()` before. */  \
    return (CTYPE const*) p_metadata->v_data_read;                    \
  }                                                                   \
                                                                      \
  /* Provide a readonly view into the data at the right offset */     \
  r_obj* data = r_altrep_data1(x);                                    \
  CTYPE const* v_data_read = CBEGIN(data) + p_metadata->start;        \
                                                                      \
  /* Set it in the metadata to save some future indirection cost */   \
  p_metadata->v_data_read = v_data_read;                              \
                                                                      \
  return v_data_read

static inline int const* r_lgl_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(int, r_lgl_cbegin);
}
static inline int const* r_int_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(int, r_int_cbegin);
}
static inline double const* r_dbl_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(double, r_dbl_cbegin);
}
static inline r_complex const* r_cpl_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_complex, r_cpl_cbegin);
}
static inline r_byte const* r_raw_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_byte, r_raw_cbegin0);
}
static inline r_obj* const* r_chr_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_obj*, r_chr_cbegin);
}
static inline r_obj* const* r_list_view_dataptr_readonly(r_obj* x) {
  R_VIEW_DATAPTR_READONLY(r_obj*, r_list_cbegin);
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
  r_obj* metadata = r_altrep_data2(x);
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);
  return p_metadata->size;
}

// -----------------------------------------------------------------------------

static inline Rboolean r_view_inspect0(
    const char* name,
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  r_obj* metadata = r_altrep_data2(x);
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);
  Rprintf("%s (materialized=%s)\n", name, p_metadata->materialized ? "T" : "F");
  return TRUE;
}

static Rboolean r_lgl_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect0(
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
  return r_view_inspect0(
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
  return r_view_inspect0(
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
  return r_view_inspect0(
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
  return r_view_inspect0(
      "altrep_raw_view", x, pre, deep, pvec, inspect_subtree
  );
}
static Rboolean r_chr_view_inspect(
    r_obj* x,
    int pre,
    int deep,
    int pvec,
    void (*inspect_subtree)(r_obj*, int, int, int)
) {
  return r_view_inspect0(
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
  return r_view_inspect0(
      "altrep_list_view", x, pre, deep, pvec, inspect_subtree
  );
}

Rboolean r_view_inspect(r_obj* x) {
  switch (r_typeof(x)) {
    case R_TYPE_logical:
      return r_lgl_view_inspect(x, 0, 0, 0, NULL);
    case R_TYPE_integer:
      return r_int_view_inspect(x, 0, 0, 0, NULL);
    case R_TYPE_double:
      return r_dbl_view_inspect(x, 0, 0, 0, NULL);
    case R_TYPE_complex:
      return r_cpl_view_inspect(x, 0, 0, 0, NULL);
    case R_TYPE_raw:
      return r_raw_view_inspect(x, 0, 0, 0, NULL);
    case R_TYPE_character:
      return r_chr_view_inspect(x, 0, 0, 0, NULL);
    case R_TYPE_list:
      return r_list_view_inspect(x, 0, 0, 0, NULL);
    default:
      r_stop_internal("Type not implemented.");
  }
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

// Particularly useful to have `v_data_read` here, because the default
// method for `Extract_subset` uses `*_ELT()` repeatedly to get each element,
// so we want as little indirection as possible here.
#define R_VIEW_ELT(CTYPE, ELT)                                             \
  r_obj* metadata = r_altrep_data2(x);                                     \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);              \
                                                                           \
  if (p_metadata->v_data_read != NULL) {                                   \
    /* This is the typical case. Only unset if the original object */      \
    /* was ALTREP and we haven't requested `DATAPTR_RO()` before or */     \
    /* materialized the view. */                                           \
    CTYPE const* v_data_read = (CTYPE const*) p_metadata->v_data_read;     \
    return v_data_read[i];                                                 \
  }                                                                        \
                                                                           \
  /* Element comes from original data that was also ALTREP. */             \
  /* If we had materialized already, `v_data_read` would have been set. */ \
  r_obj* data = r_altrep_data1(x);                                         \
                                                                           \
  return ELT(data, p_metadata->start + i)

static int r_lgl_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(int, LOGICAL_ELT);
}
static int r_int_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(int, INTEGER_ELT);
}
static double r_dbl_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(double, REAL_ELT);
}
static r_complex r_cpl_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(r_complex, COMPLEX_ELT);
}
static r_byte r_raw_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(r_byte, RAW_ELT);
}
static r_obj* r_chr_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(r_obj*, STRING_ELT);
}
static r_obj* r_list_view_elt(r_obj* x, r_ssize i) {
  R_VIEW_ELT(r_obj*, VECTOR_ELT);
}

// -----------------------------------------------------------------------------

#define R_VIEW_SET_ELT(MATERIALIZE, POKE)                                  \
  r_obj* metadata = r_altrep_data2(x);                                     \
  struct r_view_metadata* p_metadata = r_raw_begin(metadata);              \
                                                                           \
  if (p_metadata->materialized) {                                          \
    /* Already materialized */                                             \
    r_obj* data = r_altrep_data1(x);                                       \
    POKE(data, i, elt);                                                    \
  } else {                                                                 \
    /* Materialize so we can set the element. */                           \
    /* Only protect `elt` when we materialize, for performance. */         \
    /* (although gc is disabled here anyways by `ALT<TYPE>_SET_ELT()`). */ \
    KEEP(elt);                                                             \
    r_obj* data = MATERIALIZE(x);                                          \
    POKE(data, i, elt);                                                    \
    FREE(1);                                                               \
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
// which we think is good enough due to how we cache the readonly pointer.
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
