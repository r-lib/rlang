#include <string.h>
#include "rlang.h"


r_ssize r_chr_detect_index(sexp* chr, const char* c_string) {
  r_ssize n = r_length(chr);

  for (r_ssize i = 0; i != n; ++i) {
    const char* cur = CHAR(STRING_ELT(chr, i));
    if (strcmp(cur, c_string) == 0) {
      return i;
    }
  }

  return -1;
}
bool r_chr_has(sexp* chr, const char* c_string) {
  r_ssize idx = r_chr_detect_index(chr, c_string);
  return idx >= 0;
}

bool r_chr_has_any(sexp* chr, const char** c_strings) {
  r_ssize n = r_length(chr);

  for (r_ssize i = 0; i != n; ++i) {
    const char* cur = CHAR(STRING_ELT(chr, i));

    while (*c_strings) {
      if (strcmp(cur, *c_strings) == 0) {
        return true;
      }
      ++c_strings;
    }
  }

  return false;
}

void r_chr_fill(sexp* chr, sexp* value) {
  r_ssize n = r_length(chr);
  for (r_ssize i = 0; i < n; ++i) {
    SET_STRING_ELT(chr, i, value);
  }
}

static void validate_chr_setter(sexp* chr, sexp* r_string) {
  if (r_typeof(chr) != r_type_character) {
    r_abort("`chr` must be a character vector");
  }
  if (r_typeof(r_string) != r_type_string) {
    r_abort("`r_string` must be an internal R string");
  }
}

// From rlang/vec.c
void r_vec_poke_n(sexp* x, r_ssize offset,
                  sexp* y, r_ssize from, r_ssize n);

sexp* chr_prepend(sexp* chr, sexp* r_string) {
  if (r_is_null(chr)) {
    return r_str_as_character(r_string);
  } else {
    validate_chr_setter(chr, r_string);
  }

  int n = r_length(chr);
  sexp* out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 1, chr, 0, n);
  r_chr_poke(out, 0, r_string);

  FREE(1);
  return out;
}
sexp* chr_append(sexp* chr, sexp* r_str) {
  if (r_is_null(chr)) {
    return r_str_as_character(r_str);
  }
  validate_chr_setter(chr, r_str);

  int n = r_length(chr);
  sexp* out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 0, chr, 0, n);
  r_chr_poke(out, n, r_str);

  FREE(1);
  return out;
}
