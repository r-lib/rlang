#include <string.h>
#include "rlang.h"

r_obj* ffi_raw_deparse_str(r_obj* x, r_obj* prefix, r_obj* suffix) {
  if (r_typeof(x) != R_TYPE_raw) {
    r_abort("`x` must be a raw vector.");
  }
  const unsigned char* p_x = r_raw_begin(x);
  r_ssize len_data = r_length(x);

  const char* s_prefix = "";
  r_ssize len_prefix = 0;
  if (prefix != r_null) {
    if (!r_is_string(prefix)) {
      r_abort("`prefix` must be a string or NULL.");
    }
    s_prefix = r_chr_get_c_string(prefix, 0);
    len_prefix = strlen(s_prefix);
  }

  const char* s_suffix = "";
  r_ssize len_suffix = 0;
  if (suffix != r_null) {
    if (!r_is_string(suffix)) {
      r_abort("`suffix` must be a string or NULL.");
    }
    s_suffix = r_chr_get_c_string(suffix, 0);
    len_suffix = strlen(s_suffix);
  }

  r_ssize len = len_prefix + (2 * len_data) + len_suffix;

  r_obj* buf = KEEP(r_alloc_raw(len));
  char* p_buf = (char*) r_raw_begin(buf);

  r_memcpy(p_buf, s_prefix, len_prefix);
  p_buf += len_prefix;

  const char* lookup = "0123456789abcdef";

  for (r_ssize i = 0; i < len_data; ++i) {
    unsigned char value = p_x[i];
    *p_buf++ = lookup[value / 16];
    *p_buf++ = lookup[value % 16];
  }

  r_memcpy(p_buf, s_suffix, len_suffix);
  p_buf += len_suffix;

  // Invariant: p_buf == r_raw_begin(buf) + len

  r_obj* chr_out = KEEP(Rf_mkCharLenCE((char*) r_raw_begin(buf), len, CE_UTF8));
  r_obj* out = KEEP(r_str_as_character(chr_out));

  FREE(3);
  return(out);
}
