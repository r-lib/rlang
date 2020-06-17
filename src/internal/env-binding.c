#include <rlang.h>


sexp* rlang_env_get(sexp* env, sexp* nm) {
  sexp* sym = Rf_installChar(r_chr_get(nm, 0));
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

sexp* rlang_env_has(sexp* env, sexp* nms, sexp* inherit) {
  if (r_typeof(nms) != r_type_character) {
    r_abort("`nms` must be a character vector.");
  }
  if (r_typeof(inherit) != r_type_logical) {
    r_abort("`inherit` must be a logical value.");
  }

  bool c_inherits = r_lgl_get(inherit, 0);
  bool (*env_has)(sexp*, sexp*) = c_inherits ? &r_env_has_anywhere : &r_env_has;

  r_ssize n = r_length(nms);
  sexp* out = KEEP(r_new_vector(r_type_logical, n));

  int* p_out = r_lgl_deref(out);
  sexp* const * p_nms = r_chr_deref(nms);

  for (r_ssize i = 0; i < n; ++i) {
    sexp* sym = r_str_as_symbol(p_nms[i]);
    p_out[i] = env_has(env, sym);
  }

  r_poke_names(out, nms);
  FREE(1);
  return out;
}
