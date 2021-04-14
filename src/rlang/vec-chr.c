#include <string.h>
#include "rlang.h"


r_ssize r_chr_detect_index(r_obj* chr, const char* c_string) {
  r_ssize n = r_length(chr);

  for (r_ssize i = 0; i != n; ++i) {
    const char* cur = CHAR(r_chr_get(chr, i));
    if (strcmp(cur, c_string) == 0) {
      return i;
    }
  }

  return -1;
}
bool r_chr_has(r_obj* chr, const char* c_string) {
  r_ssize idx = r_chr_detect_index(chr, c_string);
  return idx >= 0;
}

bool r_chr_has_any(r_obj* chr, const char** c_strings) {
  r_ssize n = r_length(chr);

  for (r_ssize i = 0; i != n; ++i) {
    const char* cur = CHAR(r_chr_get(chr, i));

    while (*c_strings) {
      if (strcmp(cur, *c_strings) == 0) {
        return true;
      }
      ++c_strings;
    }
  }

  return false;
}

void r_chr_fill(r_obj* chr, r_obj* value, r_ssize n) {
  for (r_ssize i = 0; i < n; ++i) {
    r_chr_poke(chr, i, value);
  }
}

static void validate_chr_setter(r_obj* chr, r_obj* r_string) {
  if (r_typeof(chr) != R_TYPE_character) {
    r_abort("`chr` must be a character vector");
  }
  if (r_typeof(r_string) != R_TYPE_string) {
    r_abort("`r_string` must be an internal R string");
  }
}

// From rlang/vec.c
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n);

r_obj* chr_prepend(r_obj* chr, r_obj* r_string) {
  if (chr == r_null) {
    return r_str_as_character(r_string);
  } else {
    validate_chr_setter(chr, r_string);
  }

  int n = r_length(chr);
  r_obj* out = KEEP(r_alloc_character(n + 1));

  r_vec_poke_n(out, 1, chr, 0, n);
  r_chr_poke(out, 0, r_string);

  FREE(1);
  return out;
}
r_obj* chr_append(r_obj* chr, r_obj* r_str) {
  if (chr == r_null) {
    return r_str_as_character(r_str);
  }
  validate_chr_setter(chr, r_str);

  int n = r_length(chr);
  r_obj* out = KEEP(r_alloc_character(n + 1));

  r_vec_poke_n(out, 0, chr, 0, n);
  r_chr_poke(out, n, r_str);

  FREE(1);
  return out;
}
