#include <rlang.h>


sexp* rlang_env_get(sexp* env, sexp* nm) {
  sexp* sym = r_sym(r_chr_get_c_string(nm, 0));
  sexp* out = KEEP(r_env_find(env, sym));

  // Trigger `symbol not found` error if needed
  if (out == r_unbound_sym) {
    r_eval(sym, r_empty_env);
    r_abort("Internal error: `rlang_env_get()` should have failed earlier");
  }

  if (r_typeof(out) == r_type_promise) {
    out = r_eval(out, r_empty_env);
  }

  FREE(1);
  return out;
}
