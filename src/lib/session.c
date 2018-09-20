#include "rlang.h"

static sexp* eval_with_x(sexp* call, sexp* x);


static sexp* is_installed_call = NULL;

bool r_is_installed(const char* pkg) {
  sexp* installed = eval_with_x(is_installed_call, KEEP(r_scalar_chr(pkg)));
  bool out = r_lgl_deref(installed);

  FREE(1);
  return out;
}


void r_init_library_session() {
  is_installed_call = r_parse("requireNamespace(x, quietly = TRUE)");
  r_mark_precious(is_installed_call);
}
