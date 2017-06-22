#include "rlang.h"
#include <R_ext/GraphicsEngine.h>
#include <string.h>
#include <stdlib.h>

#define attribute_hidden

// Interface functions ---------------------------------------------------------

void copy_character(SEXP tgt, SEXP src, R_xlen_t len);
R_xlen_t unescape_character_in_copy(SEXP tgt, SEXP src, R_xlen_t i);
SEXP unescape_sexp(SEXP name);

SEXP rlang_symbol(SEXP chr) {
  SEXP string = STRING_ELT(chr, 0);
  return Rf_install(Rf_translateChar(string));
}

SEXP rlang_symbol_to_character(SEXP chr) {
  SEXP name = PRINTNAME(chr);
  return Rf_ScalarString(unescape_sexp(name));
}

SEXP rlang_unescape_character(SEXP chr) {
  R_xlen_t len = Rf_xlength(chr);
  R_xlen_t i = unescape_character_in_copy(R_NilValue, chr, 0);
  if (i == len) return chr;

  SEXP ret = KEEP(Rf_allocVector(STRSXP, len));
  copy_character(ret, chr, i);
  unescape_character_in_copy(ret, chr, i);
  FREE(1);
  return ret;
}

// Private functions -----------------------------------------------------------

SEXP unescape_char_to_sexp(char* tmp);
bool has_unicode_escape(const char* chr);
int unescape_char(char* chr);
int unescape_char_found(char* chr);
int process_byte(char* tgt, char* const src, int* len_processed);
bool has_codepoint(const char* src);
bool is_hex(const char chr);

void copy_character(SEXP tgt, SEXP src, R_xlen_t len) {
  for (int i = 0; i < len; ++i) {
    SET_STRING_ELT(tgt, i, STRING_ELT(src, i));
  }
}

R_xlen_t attribute_hidden unescape_character_in_copy(SEXP tgt, SEXP src, R_xlen_t i) {
  R_xlen_t len = r_length(src);
  int dry_run = Rf_isNull(tgt);

  for (; i < len; ++i) {
    SEXP old_elt = STRING_ELT(src, i);
    SEXP new_elt = unescape_sexp(old_elt);
    if (dry_run) {
      if (old_elt != new_elt) return i;
    } else {
      SET_STRING_ELT(tgt, i, new_elt);
    }
  }

  return i;
}

SEXP attribute_hidden unescape_sexp(SEXP name) {
  int ce = Rf_getCharCE(name);
  const char* src = CHAR(name);
  const char* re_enc = Rf_reEnc(src, ce, CE_UTF8, 0);

  if (re_enc != src) {
    // If the string has been copied, it's safe to use as buffer
    char* tmp = (char*)re_enc;
    return unescape_char_to_sexp(tmp);
  } else {
    // If not, we're in a UTF-8 locale
    // Need to check first if the string has any UTF-8 escapes
    if (!has_unicode_escape(src)) return name;
    int orig_len = strlen(re_enc);
    char tmp[orig_len + 1];
    memcpy(tmp, re_enc, orig_len + 1);
    return unescape_char_to_sexp(tmp);
  }
}

SEXP attribute_hidden unescape_char_to_sexp(char* tmp) {
  int len = unescape_char(tmp);
  return Rf_mkCharLenCE(tmp, len, CE_UTF8);
}

bool attribute_hidden has_unicode_escape(const char* chr) {
  while (*chr) {
    if (has_codepoint(chr)) {
      return true;
    }
    ++chr;
  }

  return false;
}

int attribute_hidden unescape_char(char* chr) {
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

int attribute_hidden unescape_char_found(char* chr) {
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

int attribute_hidden process_byte(char* tgt, char* const src, int* len_processed) {
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

bool attribute_hidden has_codepoint(const char* src) {
  if (src[0] != '<') return false;
  if (src[1] != 'U') return false;
  if (src[2] != '+') return false;
  for (int i = 3; i < 7; ++i) {
    if (!is_hex(src[i])) return false;
  }
  if (src[7] != '>') return false;
  return true;
}

bool attribute_hidden is_hex(const char chr) {
  if (chr >= '0' && chr <= '9') return true;
  if (chr >= 'A' && chr <= 'F') return true;
  return false;
}
