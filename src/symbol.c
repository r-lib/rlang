#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <stdbool.h>

#define attribute_hidden

SEXP unescape_character(SEXP chr);
SEXP recode_sexp(SEXP name);
bool has_unicode_escape(const char* chr);
int unescape_unicode(char* chr);
int unescape_unicode_found(char* chr);
int process_byte(char* tgt, char* const src, int* len_processed);
bool has_codepoint(const char* src);
bool is_hex(const char chr);

SEXP rlang_symbol(SEXP chr) {
  SEXP string = STRING_ELT(chr, 0);
  return Rf_install(Rf_translateChar(string));
}

SEXP rlang_symbol_to_character(SEXP chr) {
  SEXP name = PRINTNAME(chr);
  return Rf_ScalarString(recode_sexp(name));
}

SEXP rlang_unescape_names(SEXP chr) {
  SEXP names = Rf_getAttrib(chr, R_NamesSymbol);
  if (Rf_isNull(names)) return chr;
  SEXP new_names = unescape_character(names);
  if (names == new_names) return chr;
  PROTECT(new_names);
  SEXP ret = Rf_setAttrib(chr, R_NamesSymbol, new_names);
  UNPROTECT(1);
  return ret;
}

SEXP attribute_hidden unescape_character(SEXP chr) {
  R_xlen_t len = Rf_length(chr);
  R_xlen_t i;
  for (i = 0; i < len; ++i) {
    SEXP old_elt = STRING_ELT(chr, i);
    SEXP new_elt = recode_sexp(old_elt);
    if (old_elt != new_elt) break;
  }

  if (i == len) return chr;

  SEXP ret = Rf_allocVector(STRSXP, len);
  for (int j = 0; j < i; ++j) {
    SET_STRING_ELT(ret, j, STRING_ELT(chr, j));
  }
  for (; i < len; ++i) {
    SEXP new_elt = recode_sexp(STRING_ELT(chr, i));
    SET_STRING_ELT(ret, i, new_elt);
  }

  return ret;
}

SEXP attribute_hidden recode_sexp(SEXP name) {
  int ce = Rf_getCharCE(name);
  const char* src = CHAR(name);
  const char* re_enc = Rf_reEnc(src, ce, CE_UTF8, 0);

  char* tmp;
  if (re_enc != src) {
    // If the string has been copied, it's safe to use as buffer
    tmp = (char*)re_enc;
  }
  else {
    // If not, we're in a UTF-8 locale
    // Need to check first if the string has any UTF-8 escapes
    if (!has_unicode_escape(src)) return name;
    int orig_len = strlen(re_enc);
    tmp = alloca(orig_len + 1);
    memcpy(tmp, re_enc, orig_len + 1);
  }

  int len = unescape_unicode(tmp);
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

int attribute_hidden unescape_unicode(char* chr) {
  int len = 0;

  while (*chr) {
    if (has_codepoint(chr)) {
      return len + unescape_unicode_found(chr);
    }
    else {
      ++chr;
      ++len;
    }
  }

  return len;
}

int attribute_hidden unescape_unicode_found(char* chr) {
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

  int codepoint = strtol(src + strlen("<U+"), NULL, 16);
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
