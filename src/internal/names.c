#include <rlang.h>
#include <ctype.h>
#include <stdlib.h>

#include "decl/names-decl.h"
#include "utils.h"

// 3 leading '.' + 1 trailing '\0' + 24 characters
#define MAX_IOTA_SIZE 28


r_obj* ffi_names_as_unique(r_obj* names, r_obj* quiet) {
  return names_as_unique(names, r_lgl_get(quiet, 0));
}

// [[ export() ]]
r_obj* names_as_unique(r_obj* names, bool quiet) {
  if (is_unique_names(names) && !any_has_suffix(names)) {
    return names;
  }

  r_ssize n = r_length(names);

  r_obj* new_names = KEEP(r_clone(names));
  r_obj* const * v_new_names = r_chr_cbegin(new_names);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = v_new_names[i];

    // Set `NA` and dots values to "" so they get replaced by `...n`
    // later on
    if (needs_suffix(elt)) {
      r_chr_poke(new_names, i, r_strs.empty);
      continue;
    }

    // Strip `...n` suffixes
    const char* nm = r_str_c_string(elt);
    int pos = suffix_pos(nm);
    if (pos >= 0) {
      elt = Rf_mkCharLenCE(nm, pos, Rf_getCharCE(elt));
      r_chr_poke(new_names, i, elt);
      continue;
    }
  }

  // Append all duplicates with a suffix

  r_obj* dups = KEEP(chr_detect_dups(new_names));
  const int* dups_ptr = r_lgl_cbegin(dups);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = v_new_names[i];

    if (elt != r_strs.empty && !dups_ptr[i]) {
      continue;
    }

    const char* name = r_str_c_string(elt);

    int size = strlen(name);
    int buf_size = size + MAX_IOTA_SIZE;

    R_CheckStack2(buf_size);
    char buf[buf_size];
    buf[0] = '\0';

    memcpy(buf, name, size);
    int remaining = buf_size - size;

    int needed = snprintf(buf + size,
                          remaining,
                          "...%" R_PRI_SSIZE,
                          i + 1);
    if (needed >= remaining) {
      stop_large_name();
    }

    r_chr_poke(new_names, i, Rf_mkCharLenCE(buf, size + needed, Rf_getCharCE(elt)));
  }

  if (!quiet) {
    names_inform_repair(names, new_names);
  }

  FREE(2);
  return new_names;
}

static
bool is_unique_names(r_obj* names) {
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector.");
  }

  r_ssize n = r_length(names);
  r_obj* const * v_names = r_chr_cbegin(names);

  if (Rf_any_duplicated(names, FALSE)) {
    return false;
  }

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* elt = v_names[i];

    if (needs_suffix(elt)) {
      return false;
    }
  }

  return true;
}

static
bool any_has_suffix(r_obj* names) {
  r_ssize n = r_length(names);
  r_obj* const * v_names = r_chr_cbegin(names);

  for (r_ssize i = 0; i < n; ++i) {
    const char* elt = r_str_c_string(v_names[i]);

    if (suffix_pos(elt) >= 0) {
      return true;
    }
  }

  return false;
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
      r_stop_internal("Unexpected state.");
    }}

 done:
  if (suffix_end) {
    return suffix_end - name;
  } else {
    return -1;
  }
}

static
bool needs_suffix(r_obj* str) {
  return
    str == r_strs.na ||
    str == r_strs.dots ||
    str == r_strs.empty ||
    is_dotdotint(r_str_c_string(str));
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
void names_inform_repair(r_obj* old_names, r_obj* new_names) {
  r_obj* call = KEEP(r_call3(r_sym("names_inform_repair"), old_names, new_names));
  r_eval(call, rlang_ns_env);
  FREE(1);
}

static
void stop_large_name(void) {
  r_abort("Can't tidy up name because it is too large.");
}
