#include <rlang.h>
#include "decl/encoding-decl.h"

/*
 * Recursively encode character vectors as UTF-8
 *
 * A CHARSXP is left untouched if:
 * - It is the NA_STRING
 * - It is ASCII, which means the encoding will be "unknown", but is valid UTF-8
 * - It is marked as UTF-8
 *
 * Attributes are re-encoded as well.
 *
 * ASCII strings will never get marked with an encoding when they go
 * through `Rf_mkCharLenCE()`, but they will get marked as ASCII. Since
 * UTF-8 is fully compatible with ASCII, they are treated like UTF-8.
 *
 * This converts vectors that are completely marked as Latin-1 to UTF-8, rather
 * than leaving them as Latin-1. This ensures that two vectors can be compared
 * consistently if they have both been re-encoded.
 *
 * Bytes-encoded vectors are not supported, as they cannot be
 * converted to UTF-8 by `Rf_translateCharUTF8()`.
 *
 * This function never modifies `x` in place.
 */
r_obj* obj_encode_utf8(r_obj* x) {
  r_obj* out;

  switch (r_typeof(x)) {
  case R_TYPE_character: out = chr_encode_utf8(x); break;
  case R_TYPE_list: out = list_encode_utf8(x); break;
  default: out = x; break;
  }

  if (r_attrib_has_any(out)) {
    // Only `KEEP()` if there are attributes
    KEEP(out);
    // Pass down ownership to avoid a reclone if attributes change
    bool owned = x != out;
    out = obj_attrib_encode_utf8(out, owned);
    FREE(1);
  }

  return out;
}

// -----------------------------------------------------------------------------

static
r_obj* chr_encode_utf8(r_obj* x) {
  r_ssize size = r_length(x);
  r_ssize start = chr_find_encoding_start(x, size);

  if (size == start) {
    return x;
  }

  x = KEEP(r_clone(x));
  r_obj* const* p_x = r_chr_cbegin(x);

  const void* vmax = vmaxget();

  for (r_ssize i = start; i < size; ++i) {
    r_obj* const elt = p_x[i];

    if (!str_is_ascii_or_utf8(elt)) {
      r_chr_poke(x, i, str_as_utf8(elt));
    }
  }

  vmaxset(vmax);
  FREE(1);
  return x;
}

static inline
r_ssize chr_find_encoding_start(r_obj* x, r_ssize size) {
  r_obj* const* p_x = r_chr_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* const elt = p_x[i];

    if (!str_is_ascii_or_utf8(elt)) {
      return i;
    }
  }

  return size;
}

// -----------------------------------------------------------------------------

static
r_obj* list_encode_utf8(r_obj* x) {
  bool owned = false;

  r_keep_loc pi;
  KEEP_HERE(x, &pi);

  r_ssize size = r_length(x);
  r_obj* const* p_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* const elt_old = p_x[i];

    r_obj* const elt_new = obj_encode_utf8(elt_old);
    if (elt_old == elt_new) {
      continue;
    }
    KEEP(elt_new);

    if (!owned) {
      x = r_clone(x);
      KEEP_AT(x, pi);
      p_x = r_list_cbegin(x);
      owned = true;
    }

    r_list_poke(x, i, elt_new);
    FREE(1);
  }

  FREE(1);
  return x;
}

// -----------------------------------------------------------------------------

struct cb_data {
  r_obj** p_out;
  r_keep_loc shelter;
  bool* p_owned;
};

static r_obj* obj_attrib_encode_utf8_cb(r_obj* tag, r_obj* old, void* data) {
  struct cb_data* p_data = (struct cb_data*) data;

  r_obj* new = obj_encode_utf8(old);
  if (old == new) {
    return NULL;
  }
  KEEP(new);

  if (!(*p_data->p_owned)) {
    // Shallow clones `out` and its attributes
    *p_data->p_out = r_clone(*p_data->p_out);
    KEEP_AT(*p_data->p_out, p_data->shelter);
    *p_data->p_owned = true;
  }

  r_attrib_poke(*p_data->p_out, tag, new);

  FREE(1);
  return NULL;
}

static
r_obj* obj_attrib_encode_utf8(r_obj* x, bool owned) {
  // `out` pointer may be updated in place by callback
  r_obj* out = x;

  r_keep_loc shelter;
  KEEP_HERE(out, &shelter);

  struct cb_data data = {
    .p_out = &out,
    .shelter = shelter,
    .p_owned = &owned
  };

  r_attrib_map(x, obj_attrib_encode_utf8_cb, &data);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------

// String encoding normalization
// From https://github.com/r-lib/vctrs/pull/2085
static inline
bool str_is_ascii_or_utf8(r_obj* x) {
#if (R_VERSION >= R_Version(4, 5, 0))
  return Rf_charIsASCII(x) || (Rf_getCharCE(x) == CE_UTF8) || (x == r_globals.na_str);
#else
  const int mask_ascii = 8;
  const int mask_utf8 = 64;
  const int levels = LEVELS(x);
  return (levels & mask_ascii) || (levels & mask_utf8) || (x == r_globals.na_str);
#endif
}

static inline
r_obj* str_as_utf8(r_obj* x) {
  return Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8);
}
