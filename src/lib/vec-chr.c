#include <string.h>
#include "rlang.h"


r_ssize ptrs_array_length(void** ptrs) {
  r_ssize n = 0;

  while (*ptrs) {
    ++ptrs;
    ++n;
  }

  return n;
}

sexp* r_new_character(const char** strings) {
  r_ssize n = ptrs_array_length((void**) strings);
  sexp* out = KEEP(r_new_vector(STRSXP, n));

  for (r_ssize i = 0; i < n; ++i) {
    r_chr_poke(out, i, r_string(strings[i]));
  }

  FREE(1);
  return out;
}

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

static void validate_chr_setter(sexp* chr, sexp* r_string) {
  if (r_typeof(chr) != r_type_character) {
    r_abort("`chr` must be a character vector");
  }
  if (r_typeof(r_string) != r_type_string) {
    r_abort("`r_string` must be an internal R string");
  }
}
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

sexp* r_nms_are_duplicated(sexp* nms, bool from_last) {
  if (r_typeof(nms) != r_type_character) {
    r_abort("Internal error: Expected a character vector of names for checking duplication");
  }
  sexp* dups = KEEP(Rf_duplicated(nms, from_last));

  r_ssize n = r_length(dups);
  int* dups_ptr = r_lgl_deref(dups);
  sexp** nms_ptr = r_chr_deref(nms);

  for (r_ssize i = 0; i < n; ++i, ++dups_ptr, ++nms_ptr) {
    if (*nms_ptr == r_empty_str || *nms_ptr == r_missing_str) {
      *dups_ptr = false;
    }
  }

  FREE(1);
  return dups;
}


sexp* r_shared_empty_chr = NULL;
sexp* r_empty_str = NULL;

void r_init_library_vec_chr() {
  r_shared_empty_chr = r_chr("");
  r_mark_precious(r_shared_empty_chr);

  r_empty_str = r_chr_get(r_shared_empty_chr, 0);
}
