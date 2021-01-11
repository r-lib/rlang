#include "rlang.h"
#include <R_ext/Parse.h>


static void abort_parse(sexp* code, const char* why) {
  if (r_peek_option("rlang__verbose_errors") != r_null) {
    r_sexp_print(code);
  }
  r_abort("Internal error: %s", why);
}

sexp* r_parse(const char* str) {
  sexp* str_ = KEEP(r_chr(str));

  ParseStatus status;
  sexp* out = KEEP(R_ParseVector(str_, -1, &status, r_null));
  if (status != PARSE_OK) {
    abort_parse(str_, "Parsing failed");
  }
  if (r_length(out) != 1) {
    abort_parse(str_, "Expected a single expression");
  }

  out = r_list_get(out, 0);

  FREE(2);
  return out;
}
sexp* r_parse_eval(const char* str, sexp* env) {
  sexp* out = r_eval(KEEP(r_parse(str)), env);
  FREE(1);
  return out;
}
