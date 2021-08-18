#include "rlang.h"

static
r_obj* ffi_ellipsis_find_dots(r_obj* env) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("`env` is a not an environment.");
  }

  r_obj* dots = KEEP(r_env_find(env, r_syms.dots));
  if (dots == r_syms.unbound) {
    r_abort("No `...` found.");
  }

  FREE(1);
  return dots;
}

r_obj* ffi_ellipsis_dots(r_obj* env, r_obj* auto_name) {
  r_obj* dots = ffi_ellipsis_find_dots(env);

  // Empty dots
  if (dots == r_syms.missing) {
    return r_globals.empty_list;
  }

  KEEP(dots);

  bool c_auto_name = r_arg_as_bool(auto_name, "auto_name");

  int n = r_length(dots);
  r_obj* out = KEEP(r_alloc_list(n));

  r_obj* names = r_alloc_character(n);
  r_attrib_poke(out, r_syms.names, names);

  for (int i = 0; i < n; ++i) {
    r_list_poke(out, i, r_node_car(dots));

    r_obj* name = r_node_tag(dots);
    if (r_typeof(name) == R_TYPE_symbol) {
      r_chr_poke(names, i, r_sym_string(name));
    } else {
      if (c_auto_name) {
        char buffer[20];
        snprintf(buffer, 20, "..%i", i + 1);
        r_chr_poke(names, i, r_str(buffer));
      } else {
        r_chr_poke(names, i, r_globals.na_str);
      }
    }

    dots = r_node_cdr(dots);
  }

  FREE(2);
  return out;
}

static
bool ellipsis_promise_forced(r_obj* x) {
  if (r_typeof(x) != R_TYPE_promise) {
    return true;
  } else {
    return PRVALUE(x) != r_syms.unbound;
  }
}
r_obj* ffi_ellipsis_promise_forced(r_obj* x) {
  return r_lgl(ellipsis_promise_forced(x));
}

r_obj* ffi_ellipsis_dots_used(r_obj* env) {
  r_obj* dots = KEEP(ffi_ellipsis_find_dots(env));

  if (dots == r_syms.missing) {
    FREE(1);
    return r_true;
  }

  while (dots != r_null) {
    r_obj* elt = r_node_car(dots);

    if (!ellipsis_promise_forced(elt)) {
      FREE(1);
      return r_false;
    }

    dots = r_node_cdr(dots);
  }

  FREE(1);
  return r_true;
}
