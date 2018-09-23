#include <rlang.h>

bool rlang__env_binding_is_promise(sexp* env, sexp* sym) {
  if (r_typeof(sym) != r_type_symbol) {
    r_abort("Internal error: Expected symbol in active binding predicate");
  }

  sexp* obj = r_env_find(env, sym);
  return r_typeof(obj) == r_type_promise && PRVALUE(obj) == r_unbound_sym;
}

sexp* rlang_env_binding_are_promise(sexp* env, sexp* syms) {
  if (r_typeof(syms) != r_type_list) {
    r_abort("Internal error: Expected list of symbols in active binding predicate");
  }

  r_ssize_t n = r_vec_length(syms);
  sexp* out = KEEP(r_new_vector(r_type_logical, n));
  int* out_array = r_lgl_deref(out);

  for (r_ssize_t i = 0; i < n; ++i) {
    sexp* sym = r_list_get(syms, i);
    out_array[i] = rlang__env_binding_is_promise(env, sym);
  }

  FREE(1);
  return out;
}

sexp* rlang_env_get(sexp* env, sexp* nm) {
  sexp* sym = r_sym(r_chr_get_c_string(nm, 0));

  // Use r_env_find() instead of r_env_get() because `nm` might
  // reference a missing argument
  sexp* out = r_env_find(env, sym);

  // Trigger `symbol not found` error if needed
  if (out == r_unbound_sym) {
    r_eval(sym, r_empty_env);
    r_abort("Internal error: `rlang_env_get()` should have failed earlier");
  }

  if (r_typeof(out) == r_type_promise) {
    out = r_eval(out, r_empty_env);
  }

  return out;
}
