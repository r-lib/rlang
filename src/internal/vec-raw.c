#include <string.h>
#include "rlang.h"

sexp* rlang_raw_deparse_str(sexp* x, sexp* prefix, sexp* suffix) {
  if (r_typeof(x) != r_type_raw) {
    r_abort("`x` must be a raw vector.");
  }
  const unsigned char* p_x = r_raw_deref(x);
  r_ssize len_data = r_length(x);

  const char* s_prefix = "";
  r_ssize len_prefix = 0;
  if (prefix != r_null) {
    if (!r_is_string(prefix, NULL)) {
      r_abort("`prefix` must be a string or NULL.");
    }
    s_prefix = r_chr_get_c_string(prefix, 0);
    len_prefix = strlen(s_prefix);
  }

  const char* s_suffix = "";
  r_ssize len_suffix = 0;
  if (suffix != r_null) {
    if (!r_is_string(suffix, NULL)) {
      r_abort("`suffix` must be a string or NULL.");
    }
    s_suffix = r_chr_get_c_string(suffix, 0);
    len_suffix = strlen(s_suffix);
  }

  r_ssize len = len_prefix + (2 * len_data) + len_suffix;

  sexp* buf = KEEP(r_new_vector(r_type_raw, len));
  char* p_buf = (char*) r_raw_deref(buf);

  memcpy(p_buf, s_prefix, len_prefix);
  p_buf += len_prefix;

  const char* lookup = "0123456789abcdef";

  for (r_ssize i = 0; i < len_data; ++i) {
    r_byte_t value = p_x[i];
    *p_buf++ = lookup[value / 16];
    *p_buf++ = lookup[value % 16];
  }

  memcpy(p_buf, s_suffix, len_suffix);
  p_buf += len_suffix;

  // Invariant: p_buf == r_raw_deref(buf) + len

  sexp* chr_out = KEEP(Rf_mkCharLenCE((char*) r_raw_deref(buf), len, CE_UTF8));
  sexp* out = KEEP(r_str_as_character(chr_out));

  FREE(3);
  return(out);
}
