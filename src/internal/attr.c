#include <rlang.h>
#include "internal.h"
#include "vec.h"

static r_obj* c_fn = NULL;
static r_obj* as_character_call = NULL;
static r_obj* names_call = NULL;
static r_obj* set_names_call = NULL;
static r_obj* length_call = NULL;


static r_obj* node_names(r_obj* x);
static r_obj* names_dispatch(r_obj* x, r_obj* env);

r_obj* ffi_names2(r_obj* x, r_obj* env) {
  const enum r_type type = r_typeof(x);

  if (type == R_TYPE_environment) {
    r_abort("Use `env_names()` for environments.");
  }

  // Handle pairlists and language objects specially like `getAttrib()`
  // does. `r_names()` will not find these names because it has a guarantee
  // to never allocate.
  if (type == R_TYPE_pairlist || type == R_TYPE_call) {
    return node_names(x);
  }

  r_obj* nms;
  if (r_is_object(x)) {
    nms = KEEP(names_dispatch(x, env));
  } else {
    nms = KEEP(r_names(x));
  }

  if (nms == r_null) {
    r_ssize n = r_length(x);
    nms = KEEP(r_alloc_character(n));
    r_chr_fill(nms, r_strs.empty, n);
  } else {
    nms = KEEP(ffi_replace_na(nms, r_chrs.empty_string));
  }

  FREE(2);
  return nms;
}

static
r_obj* node_names(r_obj* x) {
  r_ssize n = r_length(x);

  r_obj* out = KEEP(r_alloc_character(n));

  int i = 0;

  for(; x != r_null; x = r_node_cdr(x), ++i) {
    r_obj* tag = r_node_tag(x);

    if (tag == r_null) {
      r_chr_poke(out, i, r_strs.empty);
    } else {
      r_chr_poke(out, i, PRINTNAME(tag));
    }
  }

  FREE(1);
  return out;
}

static inline r_obj* eval_fn_dots(r_obj* fn, r_obj* x, r_obj* dots, r_obj* env);
static inline r_obj* eval_as_character(r_obj* x, r_obj* env);
static inline r_obj* set_names_dispatch(r_obj* x, r_obj* nm, r_obj* env);
static inline r_ssize length_dispatch(r_obj* x, r_obj* env);

r_obj* ffi_set_names(r_obj* x, r_obj* mold, r_obj* nm, r_obj* env) {
  int n_kept = 0;

  r_obj* dots = KEEP_N(rlang_dots(env), &n_kept);

  if (!r_is_vector(x, -1)) {
    r_abort("`x` must be a vector");
  }

  if (nm == r_null) {
    x = set_names_dispatch(x, r_null, env);

    FREE(n_kept);
    return x;
  }

  if (r_is_function(nm) || r_is_formula(nm, -1, -1)) {
    if (r_names(mold) == r_null) {
      mold = KEEP_N(eval_as_character(mold, env), &n_kept);
    } else {
      mold = KEEP_N(ffi_names2(mold, env), &n_kept);
    }

    nm = KEEP_N(rlang_as_function(nm, env), &n_kept);
    nm = KEEP_N(eval_fn_dots(nm, mold, dots, env), &n_kept);
  } else {
    if (r_length(dots) > 0) {
      nm = KEEP_N(eval_fn_dots(c_fn, nm, dots, env), &n_kept);
    }

    nm = KEEP_N(eval_as_character(nm, env), &n_kept);
  }

  r_ssize n;
  if (r_is_object(x)) {
    n = length_dispatch(x, env);
  } else {
    n = r_length(x);
  }

  if (r_typeof(nm) != R_TYPE_character) {
    r_abort("`nm` must be `NULL` or a character vector.");
  }

  r_ssize nm_n = r_length(nm);
  if (nm_n != n) {
    if (nm_n != 1) {
      r_abort("The size of `nm` (%d) must be compatible with the size of `x` (%d).",
              nm_n, n);
    }

    // Recycle names vector of size 1.
    // TODO: ALTREP repetitions?
    r_obj* val = r_chr_get(nm, 0);
    nm = KEEP_N(r_alloc_character(n), &n_kept);
    r_chr_fill(nm, val, n);
  }

  if (!is_character(nm, n, OPTION_BOOL_null, OPTION_BOOL_null)) {
    r_abort("`nm` must be `NULL` or a character vector the same length as `x`");
  }

  x = set_names_dispatch(x, nm, env);

  FREE(n_kept);
  return x;
}

static
r_obj* eval_fn_dots(r_obj* fn, r_obj* x, r_obj* dots, r_obj* env) {
  r_obj* args = KEEP(r_new_node(r_syms.dot_x, dots));
  r_obj* call = KEEP(r_new_call(r_syms.dot_fn, args));

  // This evaluates `.fn(.x, ...)`
  // `.x` is the first input, x
  // `.fn` is the function, fn
  // The dots are a pairlist already in the call
  r_env_poke(env, r_syms.dot_x, x);
  r_env_poke(env, r_syms.dot_fn, fn);

  r_obj* out = r_eval(call, env);

  FREE(2);
  return out;
}

static inline
r_obj* eval_as_character(r_obj* x, r_obj* env) {
  r_env_poke(env, r_syms.dot_x, x);
  return r_eval(as_character_call, env);
}

static inline
r_obj* names_dispatch(r_obj* x, r_obj* env) {
  r_env_poke(env, r_syms.dot_x, x);
  return r_eval(names_call, env);
}

// Use `names<-()` rather than setting names directly with `r_attrib_poke_names()`
// for genericity and for speed. `names<-()` can shallow duplicate `x`'s
// attributes using ALTREP wrappers, which is not in R's public API.
static inline
r_obj* set_names_dispatch(r_obj* x, r_obj* nm, r_obj* env) {
  r_env_poke(env, r_syms.dot_x, x);
  r_env_poke(env, r_syms.dot_y, nm);
  return r_eval(set_names_call, env);
}

static inline
r_ssize length_dispatch(r_obj* x, r_obj* env) {
  r_env_poke(env, r_syms.dot_x, x);
  r_obj* n = KEEP(r_eval(length_call, env));

  if (r_length(n) != 1) {
    r_abort("Object length must have size 1, not %i", r_length(n));
  }

  r_ssize out;

  switch (r_typeof(n)) {
  case R_TYPE_integer:
    out = (r_ssize) r_int_begin(n)[0];
    break;
  case R_TYPE_double:
    out = r_dbl_begin(n)[0];
    break;
  default:
    r_abort("Object length has unknown type %s", r_type_as_c_string(r_typeof(n)));
  }

  FREE(1);
  return out;
}


void rlang_init_attr(r_obj* ns) {
  c_fn = r_eval(r_sym("c"), r_envs.base);

  as_character_call = r_parse("as.character(.x)");
  r_preserve(as_character_call);

  names_call = r_parse("names(.x)");
  r_preserve(names_call);

  set_names_call = r_parse("`names<-`(.x, .y)");
  r_preserve(set_names_call);

  length_call = r_parse("length(.x)");
  r_preserve(length_call);
}
