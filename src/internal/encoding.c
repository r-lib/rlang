#include <rlang.h>
#include "encoding.h"
#include "decl/encoding-decl.h"

r_obj* r_normalise_encoding(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_character: x = chr_normalise_encoding(x); break;
  case R_TYPE_list: x = list_normalise_encoding(x); break;
  default: break;
  }

  // For performance, avoid `KEEP()` / `FREE()` when not needed
  r_obj* attrib = r_attrib(x);
  if (attrib != r_null) {
    KEEP(x);
    x = r_attrib_normalise_encoding(x, attrib);
    FREE(1);
  }

  return x;
}

// -----------------------------------------------------------------------------

static
r_obj* chr_normalise_encoding(r_obj* x) {
  r_ssize size = r_length(x);
  r_ssize start = chr_find_normalise_start(x, size);

  if (size == start) {
    return x;
  }

  x = KEEP(r_clone_shared(x));
  r_obj* const* p_x = r_chr_cbegin(x);

  const void* vmax = vmaxget();

  for (r_ssize i = start; i < size; ++i) {
    r_obj* const elt = p_x[i];

    if (str_is_normalised(elt)) {
      continue;
    }

    r_chr_poke(x, i, str_normalise(elt));
  }

  vmaxset(vmax);
  FREE(1);
  return x;
}

static inline
r_ssize chr_find_normalise_start(r_obj* x, r_ssize size) {
  r_obj* const* p_x = r_chr_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* const elt = p_x[i];

    if (str_is_normalised(elt)) {
      continue;
    }

    return i;
  }

  return size;
}

// -----------------------------------------------------------------------------

static
r_obj* list_normalise_encoding(r_obj* x) {
  r_keep_t pi;
  KEEP_HERE(x, &pi);

  r_ssize size = r_length(x);
  r_obj* const* p_x = r_list_cbegin(x);

  for (r_ssize i = 0; i < size; ++i) {
    r_obj* const elt_old = p_x[i];

    r_obj* const elt_new = r_normalise_encoding(elt_old);
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
r_obj* r_attrib_normalise_encoding(r_obj* x, r_obj* attrib) {
  r_obj* attrib_new = attrib_normalise_encoding(attrib);
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
r_obj* attrib_normalise_encoding(r_obj* x) {
  r_ssize loc = 0;
  bool owned = false;

  r_keep_t pi;
  KEEP_HERE(x, &pi);

  for (r_obj* node = x; node != r_null; node = r_node_cdr(node), ++loc) {
    r_obj* elt_old = r_node_car(node);

    r_obj* elt_new = r_normalise_encoding(elt_old);
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
r_obj* str_normalise(r_obj* x) {
  return r_str(Rf_translateCharUTF8(x));
}

static inline
bool str_is_normalised(r_obj* x) {
  return str_is_ascii_or_utf8(x) || (x == NA_STRING);
}

#define MASK_ASCII 8
#define MASK_UTF8 64

// The first 128 values are ASCII, and are the same regardless of the encoding.
// Otherwise we enforce UTF-8.
static inline
bool str_is_ascii_or_utf8(r_obj* x) {
  const int levels = LEVELS(x);
  return (levels & MASK_ASCII) || (levels & MASK_UTF8);
}

#undef MASK_ASCII
#undef MASK_UTF8
