#include <rlang.h>
#include "internal.h"
#include "decl/call-decl.h"
#include "utils.h"
#include "vec.h"


r_obj* ffi_is_call(r_obj* x, r_obj* name, r_obj* ffi_n, r_obj* ns) {
  if (r_typeof(x) != R_TYPE_call) {
    return r_false;
  }

  if (ns != r_null) {
    if (!is_character(ns, -1, OPTION_BOOL_false, OPTION_BOOL_null)) {
      r_abort("`ns` must be a character vector of namespaces.");
    }

    bool found = false;
    r_obj* const * v_ns = r_chr_cbegin(ns);
    r_ssize ns_len = r_length(ns);

    for (r_ssize i = 0; i < ns_len; ++i) {
      r_obj* elt = v_ns[i];

      if ((elt == r_strs.empty && !call_is_namespaced(x, r_null))
            || call_is_namespaced(x, elt)) {
        found = true;
        break;
      }
    }

    if (!found) {
      return r_false;
    }
  }

  x = KEEP(call_unnamespace(x));

  if (name != r_null) {
    r_obj* fn = r_node_car(x);
    if (r_typeof(fn) != R_TYPE_symbol) {
      FREE(1);
      return r_false;
    }

    switch (r_typeof(name)) {
    case R_TYPE_symbol:
      if (fn == name) {
        goto found_name;
      }
      FREE(1);
      return r_false;

    // List of symbols
    case R_TYPE_list: {
      r_obj* const * v_name = r_list_cbegin(name);
      r_ssize name_len = r_length(name);

      for (r_ssize i = 0; i < name_len; ++i) {
        if (fn == v_name[i]) {
          goto found_name;
        }
      }
      FREE(1);
      return r_false;
    }

    default:
      break;
    }

    if (!is_character(name, -1, OPTION_BOOL_false, OPTION_BOOL_false)) {
      r_abort("`name` must be a character vector of names.");
    }

    fn = r_sym_string(fn);
    r_obj* const * v_name = r_chr_cbegin(name);
    r_ssize name_len = r_length(name);

    for (r_ssize i = 0; i < name_len; ++i) {
      if (fn == v_name[i]) {
        goto found_name;
      }
    }
    FREE(1);
    return r_false;
  }
 found_name:

  if (ffi_n != r_null) {
    r_ssize n = validate_n(ffi_n);
    if (!_r_has_correct_length(r_node_cdr(x), n)) {
      FREE(1);
      return r_false;
    }
  }

  FREE(1);
  return r_true;
}

static
bool call_is_namespaced(r_obj* x, r_obj* ns) {
  if (r_typeof(x) != R_TYPE_call) {
    return(false);
  }

  r_obj* car = r_node_car(x);
  if (r_typeof(car) != R_TYPE_call) {
    return(false);
  }

  if (ns != r_null) {
    r_obj* arg = r_node_cadr(car);
    if (r_typeof(arg) != R_TYPE_symbol || r_sym_string(arg) != ns) {
      return false;
    }
  }

  return r_node_car(car) == r_syms.colon2;
}

static inline
r_obj* call_unnamespace(r_obj* x) {
  if (call_is_namespaced(x, r_null)) {
    return r_new_call(r_node_cadr(r_node_cdar(x)), r_node_cdr(x));
  } else {
    return x;
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

static
bool is_callable(r_obj* x) {
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


r_obj* ffi_call_zap_inline(r_obj* x) {
  if (r_typeof(x) == R_TYPE_call) {
    r_obj* out = KEEP(r_call_clone(x));
    call_zap_inline(out);
    FREE(1);
    return out;
  } else {
    return call_zap_one(x);
  }
}

static
void call_zap_inline(r_obj* x) {
  if (r_node_car(x) == r_syms.function) {
    call_zap_fn(x);
  } else {
    node_zap_inline(x);
  }
}

static
void node_zap_inline(r_obj* x) {
  while (x != r_null) {
    r_node_poke_car(x, call_zap_one(r_node_car(x)));
    x = r_node_cdr(x);
  }
}

static
void call_zap_fn(r_obj* x) {
  // Formals
  x = r_node_cdr(x);
  node_zap_inline(r_node_car(x));

  // Body
  x = r_node_cdr(x);
  r_node_poke_car(x, call_zap_one(r_node_car(x)));

  // Zap srcref
  x = r_node_cdr(x);
  r_node_poke_car(x, r_null);
}

static
r_obj* call_zap_one(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_call:
    call_zap_inline(x);
    return x;

  case R_TYPE_null:
  case R_TYPE_symbol:
    return x;

  // Syntactic literals
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_character:
  case R_TYPE_complex: // Not entirely correct for complex
    if (r_attrib(x) == r_null && r_length(x) == 1) {
      return x;
    } else {
      return type_sum(x);
    }

  default:
    return type_sum(x);
  }
}

static
r_obj* type_sum(r_obj* x) {
  return r_eval_with_x(type_sum_call, x, rlang_ns_env);
}


void rlang_init_call(r_obj* ns) {
  type_sum_call = r_parse("call_type_sum(x)");
  r_preserve_global(type_sum_call);
}

static
r_obj* type_sum_call = NULL;
