#ifndef RLANG_VECTOR_CHR_H
#define RLANG_VECTOR_CHR_H

#include <string.h>


static inline
const char* r_str_c_string(r_obj* str) {
  return CHAR(str);
}

bool r_chr_has(r_obj* chr, const char* c_string);
bool r_chr_has_any(r_obj* chr, const char** c_strings);
r_ssize r_chr_detect_index(r_obj* chr, const char* c_string);

void r_chr_fill(r_obj* chr, r_obj* value, r_ssize n);


static inline
r_obj* r_str_as_character(r_obj* x) {
  return Rf_ScalarString(x);
}

/*
 * A symbol is always in the native encoding. This means that UTF-8
 * data frame names undergo a lossy translation when they are
 * transformed to symbols to create a data mask. To deal with this, we
 * translate all serialised unicode tags back to UTF-8. This way the
 * UTF-8 -> native -> UTF-8 translation that occurs during the
 * character -> symbol -> character conversion fundamental for data
 * masking is transparent and lossless for the end user.
 *
 * Starting from R 4.0, `installChar()` warns when translation to
 * native encoding is lossy. This warning is disruptive for us since
 * we correctly translate strings behind the scene. To work around
 * this, we call `translateChar()` which doesn't warn (at least
 * currently). If the pointers are the same, no translation is
 * needed and we can call `installChar()`, which preserves the
 * current encoding of the string. Otherwise we intern the symbol
 * with `install()` without encoding.
 */
static inline
r_obj* r_str_as_symbol(r_obj* str) {
  const char* str_native = Rf_translateChar(str);

  if (str_native == CHAR(str)) {
    return Rf_installChar(str);
  } else {
    return Rf_install(str_native);
  }
}

static inline
bool r_str_is_name(r_obj* str) {
  if (str == r_globals.na_str) {
    return false;
  }
  if (str == r_strs.empty) {
    return false;
  }
  return true;
}


#endif
