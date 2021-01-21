#include "rlang.h"

sexp* eval_with_x(sexp* call, sexp* x);


static sexp* is_installed_call = NULL;

bool r_is_installed(const char* pkg) {
  sexp* installed = eval_with_x(is_installed_call, KEEP(r_chr(pkg)));
  bool out = *r_lgl_deref(installed);

  FREE(1);
  return out;
}


static sexp* has_colour_call = NULL;

bool r_has_colour() {
  if (!r_is_installed("crayon")) {
    return false;
  }

  return *r_lgl_deref(r_eval(has_colour_call, r_base_env));
}


void r_init_library_session() {
  is_installed_call = r_parse("requireNamespace(x, quietly = TRUE)");
  r_preserve(is_installed_call);

  has_colour_call = r_parse("crayon::has_color()");
  r_preserve(has_colour_call);
}
