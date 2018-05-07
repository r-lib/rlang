#include <string.h>
#include "rlang.h"


size_t ptrs_array_length(void** ptrs) {
  size_t n = 0;

  while (*ptrs) {
    ++ptrs;
    ++n;
  }

  return n;
}

sexp* r_new_character(const char** strings) {
  size_t n = ptrs_array_length((void**) strings);
  sexp* out = KEEP(r_new_vector(STRSXP, n));

  for (int i = 0; i < n; ++i) {
    r_chr_poke(out, i, r_string(strings[i]));
  }

  FREE(1);
  return out;
}

bool r_chr_has(sexp* chr, const char* c_string) {
  size_t n = r_length(chr);

  for (int i = 0; i != n; ++i) {
    const char* cur = CHAR(STRING_ELT(chr, i));
    if (strcmp(cur, c_string) == 0) {
      return true;
    }
  }

  return false;
}

bool r_chr_has_any(sexp* chr, const char** c_strings) {
  size_t n = r_length(chr);

  for (int i = 0; i != n; ++i) {
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

static void validate_chr_setter(sexp* chr, sexp* r_string) {
  if (r_typeof(chr) != r_type_character) {
    r_abort("`chr` must be a character vector");
  }
  if (!r_is_r_string(r_string)) {
    r_abort("`r_string` must be an internal R string");
  }
}
sexp* chr_prepend(sexp* chr, sexp* r_string) {
  if (r_is_null(chr)) {
    return Rf_ScalarString(r_string);
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
    return Rf_ScalarString(r_str);
  }
  validate_chr_setter(chr, r_str);

  int n = r_length(chr);
  sexp* out = KEEP(r_new_vector(STRSXP, n + 1));

  r_vec_poke_n(out, 0, chr, 0, n);
  r_chr_poke(out, n, r_str);

  FREE(1);
  return out;
}


sexp* r_shared_empty_chr = NULL;

void r_init_library_vec_chr() {
  r_shared_empty_chr = r_scalar_chr("");
  r_mark_precious(r_shared_empty_chr);
}
