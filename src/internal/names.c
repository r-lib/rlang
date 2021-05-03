#include <rlang.h>
#include <ctype.h>
#include "internal.h"

// 3 leading '.' + 1 trailing '\0' + 24 characters
#define MAX_IOTA_SIZE 28

static
bool is_unique_names(SEXP names);

SEXP as_unique_names_impl(SEXP names, bool quiet);

static
ptrdiff_t suffix_pos(const char* name);

static
bool needs_suffix(SEXP str);

// TODO: Export as well?
static
void describe_repair(SEXP old_names, SEXP new_names);

bool any_has_suffix(SEXP names) {
  R_len_t n = Rf_length(names);
  const SEXP* names_ptr = STRING_PTR_RO(names);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = names_ptr[i];

    if (suffix_pos(CHAR(elt)) >= 0) {
      return true;
    }
  }

  return false;
}

SEXP vec_as_unique_names(SEXP names, bool quiet) {
  if (is_unique_names(names) && !any_has_suffix(names)) {
    return names;
  } else {
    return(as_unique_names_impl(names, quiet));
  }
}

SEXP ffi_chr_as_unique_names(SEXP names, SEXP quiet) {
  SEXP out = PROTECT(vec_as_unique_names(names, LOGICAL(quiet)[0]));
  UNPROTECT(1);
  return out;
}

static
bool is_unique_names(SEXP names) {
  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "`names` must be a character vector");
  }

  R_len_t n = Rf_length(names);
  const SEXP* names_ptr = STRING_PTR_RO(names);

  if (Rf_any_duplicated(names, FALSE)) {
    return false;
  }

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = names_ptr[i];

    if (needs_suffix(elt)) {
      return false;
    }
  }

  return true;
}

static
void stop_large_name() {
  Rf_errorcall(R_NilValue, "Can't tidy up name because it is too large");
}

SEXP as_unique_names_impl(SEXP names, bool quiet) {
  R_len_t n = Rf_length(names);

  SEXP new_names = PROTECT(Rf_shallow_duplicate(names));
  const SEXP* new_names_ptr = STRING_PTR_RO(new_names);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = new_names_ptr[i];

    // Set `NA` and dots values to "" so they get replaced by `...n`
    // later on
    if (needs_suffix(elt)) {
      elt = r_strs.empty;
      SET_STRING_ELT(new_names, i, elt);
      continue;
    }

    // Strip `...n` suffixes
    const char* nm = CHAR(elt);
    int pos = suffix_pos(nm);
    if (pos >= 0) {
      elt = Rf_mkCharLenCE(nm, pos, Rf_getCharCE(elt));
      SET_STRING_ELT(new_names, i, elt);
      continue;
    }
  }

  // Append all duplicates with a suffix

  SEXP dups = PROTECT(Rf_duplicated(new_names, FALSE));
  const int* dups_ptr = LOGICAL_RO(dups);

  for (R_len_t i = 0; i < n; ++i) {
    SEXP elt = new_names_ptr[i];

    if (elt != r_strs.empty && !dups_ptr[i]) {
      continue;
    }

    const char* name = CHAR(elt);

    int size = strlen(name);
    int buf_size = size + MAX_IOTA_SIZE;

    R_CheckStack2(buf_size);
    char buf[buf_size];
    buf[0] = '\0';

    memcpy(buf, name, size);
    int remaining = buf_size - size;

    int needed = snprintf(buf + size, remaining, "...%d", i + 1);
    if (needed >= remaining) {
      stop_large_name();
    }

    SET_STRING_ELT(new_names, i, Rf_mkCharLenCE(buf, size + needed, Rf_getCharCE(elt)));
  }

  if (!quiet) {
    describe_repair(names, new_names);
  }

  UNPROTECT(2);
  return new_names;
}

static
bool is_dotdotint(const char* name) {
  int n = strlen(name);

  if (n < 3) {
    return false;
  }
  if (name[0] != '.' || name[1] != '.') {
    return false;
  }

  if (name[2] == '.') {
    name += 3;
  } else {
    name += 2;
  }

  return (bool) strtol(name, NULL, 10);
}

static
ptrdiff_t suffix_pos(const char* name) {
  int n = strlen(name);

  const char* suffix_end = NULL;
  int in_dots = 0;
  bool in_digits = false;

  for (const char* ptr = name + n - 1; ptr >= name; --ptr) {
    char c = *ptr;

    if (in_digits) {
      if (c == '.') {
        in_digits = false;
        in_dots = 1;
        continue;
      }

      if (isdigit(c)) {
        continue;
      }

      goto done;
    }

    switch (in_dots) {
    case 0:
      if (isdigit(c)) {
        in_digits = true;
        continue;
      }
      goto done;
    case 1:
    case 2:
      if (c == '.') {
        ++in_dots;
        continue;
      }
      goto done;
    case 3:
      suffix_end = ptr + 1;
      if (isdigit(c)) {
        in_dots = 0;
        in_digits = true;
        continue;
      }
      goto done;

    default:
      r_stop_internal("suffix_pos", "Unexpected state.");
    }}

 done:
  if (suffix_end) {
    return suffix_end - name;
  } else {
    return -1;
  }
}

static
bool needs_suffix(SEXP str) {
  return
    str == NA_STRING ||
    str == r_strs.dots ||
    str == r_strs.empty ||
    is_dotdotint(CHAR(str));
}

static
void describe_repair(SEXP old_names, SEXP new_names) {
  SEXP call = PROTECT(Rf_lang3(Rf_install("describe_repair"),
    old_names, new_names));
  Rf_eval(call, rlang_ns_env);

  // To reset visibility when called from a `.External2()`
  Rf_eval(R_NilValue, R_EmptyEnv);

  UNPROTECT(1);
}
