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
 * If `x` is not shared (i.e. `r_is_shared(x) == false`), this function will
 * modify `x` in place. Otherwise, a copy is made.
 */
r_obj* obj_encode_utf8(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_character: x = chr_encode_utf8(x); break;
  case R_TYPE_list: x = list_encode_utf8(x); break;
  default: break;
  }

  r_obj* attrib = r_attrib(x);
  if (attrib != r_null) {
    KEEP(x);
    x = obj_attrib_encode_utf8(x, attrib);
    FREE(1);
  }

  return x;
}

// -----------------------------------------------------------------------------

static
r_obj* chr_encode_utf8(r_obj* x) {
  r_ssize size = r_length(x);
  r_ssize start = chr_find_encoding_start(x, size);

  if (size == start) {
    return x;
  }

  x = KEEP(r_clone_shared(x));
  r_obj* const* p_x = r_chr_cbegin(x);

  const void* vmax = vmaxget();

  for (r_ssize i = start; i < size; ++i) {
    r_obj* const elt = p_x[i];

    if (str_needs_encoding(elt)) {
      r_chr_poke(x, i, str_encode_utf8(elt));
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

    if (str_needs_encoding(elt)) {
      return i;
    }
  }

  return size;
}

// -----------------------------------------------------------------------------

static
r_obj* list_encode_utf8(r_obj* x) {
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

    if (r_is_shared(x)) {
      // Cloned once, at which point `x` is free of references
      x = r_clone(x);
      KEEP_AT(x, pi);
      p_x = r_list_cbegin(x);
    }

    r_list_poke(x, i, elt_new);
    FREE(1);
  }

  FREE(1);
  return x;
}

// -----------------------------------------------------------------------------

static
r_obj* obj_attrib_encode_utf8(r_obj* x, r_obj* attrib) {
  r_obj* attrib_new = attrib_encode_utf8(attrib);
  if (attrib_new == attrib) {
    return x;
  }
  KEEP(attrib_new);

  x = KEEP(r_clone_shared(x));
  r_poke_attrib(x, attrib_new);

  FREE(2);
  return x;
}

static
r_obj* attrib_encode_utf8(r_obj* x) {
  r_ssize loc = 0;
  bool owned = false;

  r_keep_loc pi;
  KEEP_HERE(x, &pi);

  for (r_obj* node = x; node != r_null; node = r_node_cdr(node), ++loc) {
    r_obj* elt_old = r_node_car(node);

    r_obj* elt_new = obj_encode_utf8(elt_old);
    if (elt_old == elt_new) {
      continue;
    }
    KEEP(elt_new);

    if (!owned) {
      // Shallow clone entire pairlist if not owned.
      // Should be fast because these are generally short.
      x = r_clone(x);
      KEEP_AT(x, pi);
      owned = true;

      node = x;

      // Restore original positioning post-clone
      for (r_ssize i = 0; i < loc; ++i) {
        node = r_node_cdr(node);
      }
    }

    r_node_poke_car(node, elt_new);
    FREE(1);
  }

  FREE(1);
  return x;
}

// -----------------------------------------------------------------------------

static inline
r_obj* str_encode_utf8(r_obj* x) {
  return r_str(Rf_translateCharUTF8(x));
}

static inline
bool str_needs_encoding(r_obj* x) {
  return (!str_is_ascii_or_utf8(x)) && (x != NA_STRING);
}

#if (R_VERSION < R_Version(4, 5, 0))

#define MASK_ASCII 8
#define MASK_UTF8 64
// The first 128 values are ASCII, and are the same regardless of the encoding.
// Otherwise we enforce UTF-8.
static inline
bool str_is_ascii_or_utf8(r_obj* x) {
  const int levels = LEVELS(x);
  return (levels & MASK_ASCII) || (levels & MASK_UTF8);
}

#else

static inline
bool str_is_ascii_or_utf8(r_obj* x) {
  return Rf_charIsUTF8(x);
}

#endif
