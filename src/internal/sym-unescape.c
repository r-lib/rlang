#include "rlang.h"
#include <R_ext/GraphicsEngine.h>
#include <string.h>
#include <stdlib.h>

#include "decl/sym-unescape-decl.h"


// Interface functions ---------------------------------------------------------

void copy_character(r_obj* tgt, r_obj* src, R_xlen_t len);
R_xlen_t unescape_character_in_copy(r_obj* tgt, r_obj* src, R_xlen_t i);

r_obj* ffi_symbol(r_obj* chr) {
  return r_str_as_symbol(r_chr_get(chr, 0));
}

r_obj* ffi_sym_as_string(r_obj* sym) {
  return str_unserialise_unicode(PRINTNAME(sym));
}

r_obj* ffi_sym_as_character(r_obj* sym) {
  r_obj* str = KEEP(ffi_sym_as_string(sym));
  r_obj* out = r_str_as_character(str);
  FREE(1);
  return out;
}

r_obj* ffi_unescape_character(r_obj* chr) {
  R_xlen_t len = Rf_xlength(chr);
  R_xlen_t i = unescape_character_in_copy(r_null, chr, 0);
  if (i == len) return chr;

  r_obj* ret = KEEP(r_alloc_character(len));
  copy_character(ret, chr, i);
  unescape_character_in_copy(ret, chr, i);
  FREE(1);
  return ret;
}

// Private functions -----------------------------------------------------------

static r_obj* unescape_char_to_sexp(char* tmp);
static bool has_unicode_escape(const char* chr);
static int unescape_char(char* chr);
static int unescape_char_found(char* chr);
static int process_byte(char* tgt, char* const src, int* len_processed);
static bool has_codepoint(const char* src);
static bool is_hex(const char chr);

void copy_character(r_obj* tgt, r_obj* src, R_xlen_t len) {
  for (int i = 0; i < len; ++i) {
    r_chr_poke(tgt, i, r_chr_get(src, i));
  }
}

R_xlen_t unescape_character_in_copy(r_obj* tgt, r_obj* src, R_xlen_t i) {
  R_xlen_t len = r_length(src);
  int dry_run = Rf_isNull(tgt);

  for (; i < len; ++i) {
    r_obj* old_elt = r_chr_get(src, i);
    r_obj* new_elt = str_unserialise_unicode(old_elt);
    if (dry_run) {
      if (old_elt != new_elt) return i;
    } else {
      r_chr_poke(tgt, i, new_elt);
    }
  }

  return i;
}

r_obj* str_unserialise_unicode(r_obj* r_string) {
  int ce = Rf_getCharCE(r_string);
  const char* src = CHAR(r_string);

  if (!has_unicode_escape(src)) {
    return r_string;
  }

  const char* re_enc = Rf_reEnc(src, ce, CE_UTF8, 0);

  if (re_enc == src) {
    // The string was not copied because we're in a UTF-8 locale.
    // Need to check first if the string has any UTF-8 escapes.
    int orig_len = strlen(re_enc);
    char tmp[orig_len + 1];
    r_memcpy(tmp, re_enc, orig_len + 1);
    return unescape_char_to_sexp(tmp);
  } else {
    // The string has been copied so it's safe to use as buffer
    char* tmp = (char*)re_enc;
    return unescape_char_to_sexp(tmp);
  }
}

static
r_obj* unescape_char_to_sexp(char* tmp) {
  int len = unescape_char(tmp);
  return Rf_mkCharLenCE(tmp, len, CE_UTF8);
}

static
bool has_unicode_escape(const char* chr) {
  while (*chr) {
    if (has_codepoint(chr)) {
      return true;
    }
    ++chr;
  }

  return false;
}

static
int unescape_char(char* chr) {
  int len = 0;

  while (*chr) {
    if (has_codepoint(chr)) {
      return len + unescape_char_found(chr);
    } else {
      ++chr;
      ++len;
    }
  }

  return len;
}

static
int unescape_char_found(char* chr) {
  char* source = chr;
  char* target = chr;
  int len = 0;

  while (*source) {
    int len_processed;
    int len_new = process_byte(target, source, &len_processed);
    source += len_processed;
    target += len_new;
    len += len_new;
  }

  *target = 0;
  return len;
}

static
int process_byte(char* tgt, char* const src, int* len_processed) {
  if (!has_codepoint(src)) {
    // Copy only the first character (angle bracket or not), advance
    *tgt = *src;
    *len_processed = 1;
    return 1;
  }

  unsigned int codepoint = strtoul(src + strlen("<U+"), NULL, 16);
  *len_processed = strlen("<U+xxxx>");

  // We have 8 bytes space, codepoints occupy less than that:
  return (int)Rf_ucstoutf8(tgt, codepoint);
}

static
bool has_codepoint(const char* src) {
  if (src[0] != '<') return false;
  if (src[1] != 'U') return false;
  if (src[2] != '+') return false;
  for (int i = 3; i < 7; ++i) {
    if (!is_hex(src[i])) return false;
  }
  if (src[7] != '>') return false;
  return true;
}

static
bool is_hex(const char chr) {
  if (chr >= '0' && chr <= '9') return true;
  if (chr >= 'A' && chr <= 'F') return true;
  return false;
}
