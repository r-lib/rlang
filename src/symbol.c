#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>
#include <stdbool.h>

#define attribute_hidden

extern Rboolean utf8locale;

SEXP recode_symbol(const char* src);
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
  const char* src = CHAR(PRINTNAME(chr));
  if (utf8locale) return Rf_ScalarString(Rf_mkCharCE(src, CE_UTF8));
  else return recode_symbol(src);
}

SEXP attribute_hidden recode_symbol(const char* src) {
  const char* re_enc = Rf_reEnc(src, CE_NATIVE, CE_UTF8, 0);

  // We know that the string has been copied, so it's safe to use it as buffer
  char* tmp = (char*)re_enc;

  int len = unescape_unicode(tmp);
  SEXP chrsxp = Rf_mkCharLenCE(tmp, len, CE_UTF8);
  return Rf_ScalarString(chrsxp);
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
