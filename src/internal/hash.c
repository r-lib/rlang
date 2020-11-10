#include <rlang.h>
#include "md5/md5.c"
#include <stdio.h> // sprintf()

static inline sexp* rlang_hash_impl(const unsigned char* data, unsigned long size);

sexp* rlang_hash_raw(sexp* x) {
  if (r_typeof(x) != r_type_raw) {
    r_abort("`x` must be a raw vector.");
  }

  const unsigned char* data = r_raw_deref(x);
  unsigned long size = (unsigned long) r_length(x);

  return rlang_hash_impl(data, size);
}

sexp* rlang_hash_string(sexp* x) {
  if (!r_is_string(x, NULL)) {
    r_abort("`x` must be a string.");
  }

  sexp* string = r_chr_get(x, 0);
  const char* c_string = r_str_deref(string);

  const unsigned char* data = (const unsigned char*) c_string;
  unsigned long size = (unsigned long) r_length(string);

  return rlang_hash_impl(data, size);
}


static inline
sexp* rlang_hash_impl(const unsigned char* data, unsigned long size) {
  unsigned char result[16];

  MD5_CTX context;
  MD5_Init(&context);
  MD5_Update(&context, data, size);
  MD5_Final(result, &context);

  // 32 for hash, 1 for terminating null added by `sprintf()`
  char out[32 + 1];

  for (unsigned int i = 0; i < 16; ++i) {
    sprintf(out + i * 2, "%02x", result[i]);
  }

  return r_chr(out);
}
