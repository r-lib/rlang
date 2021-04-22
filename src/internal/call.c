#include <rlang.h>
#include "internal.h"


static bool is_callable(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_symbol:
  case R_TYPE_call:
  case R_TYPE_closure:
  case R_TYPE_builtin:
  case R_TYPE_special:
    return true;
  default:
    return false;
  }
}

r_obj* rlang_call2(r_obj* fn, r_obj* args, r_obj* ns) {
  if (r_typeof(fn) == R_TYPE_character) {
    if (r_length(fn) != 1) {
      r_abort("`.fn` must be a string, a symbol, a call, or a function");
    }
    fn = r_sym(r_chr_get_c_string(fn, 0));
  } else if (!is_callable(fn)) {
    r_abort("Can't create call to non-callable object");
  }

  int n_kept = 0;

  if (ns != r_null) {
    if (!r_is_string(ns)) {
      r_abort("`ns` must be a string");
    }
    if (r_typeof(fn) != R_TYPE_symbol) {
      r_abort("`fn` must be a string or symbol when a namespace is supplied");
    }
    ns = r_sym(r_chr_get_c_string(ns, 0));
    fn = KEEP_N(r_call3(r_syms.colon2, ns, fn), &n_kept);
  }

  r_obj* out = r_new_call(fn, args);

  FREE(n_kept);
  return out;
}

r_obj* ffi_call2(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);

  r_obj* fn = KEEP(r_eval(r_sym(".fn"), env));
  r_obj* ns = KEEP(r_eval(r_sym(".ns"), env));
  r_obj* dots = KEEP(rlang_dots(env));

  r_obj* out = rlang_call2(fn, dots, ns);

  FREE(3);
  return out;
}
