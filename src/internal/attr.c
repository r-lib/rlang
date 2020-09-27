#include <rlang.h>
#include "internal.h"

static inline sexp* r_node_names(sexp* x);
static inline sexp* r_names_dispatch(sexp* x, sexp* env);

sexp* rlang_names2(sexp* x, sexp* env) {
  const enum r_type type = r_typeof(x);

  if (type == r_type_environment) {
    r_abort("Use `env_names()` for environments.");
  }

  // Handle pairlists and language objects specially like `getAttrib()`
  // does. `r_names()` will not find these names because it has a guarantee
  // to never allocate.
  if (type == r_type_pairlist || type == r_type_call) {
    return r_node_names(x);
  }

  sexp* nms;
  if (r_is_object(x)) {
    nms = KEEP(r_names_dispatch(x, env));
  } else {
    nms = KEEP(r_names(x));
  }

  if (r_is_null(nms)) {
    r_ssize n = r_length(x);
    nms = KEEP(r_new_vector(r_type_character, n));
    r_chr_fill(nms, r_empty_str, n);
  } else {
    nms = KEEP(rlang_replace_na(nms, r_shared_empty_chr));
  }

  FREE(2);
  return nms;
}

static inline sexp* r_node_names(sexp* x) {
  r_ssize n = r_length(x);

  sexp* out = KEEP(r_new_vector(r_type_character, n));

  int i = 0;

  for(; x != r_null; x = r_node_cdr(x), ++i) {
    sexp* tag = r_node_tag(x);

    if (tag == r_null) {
      SET_STRING_ELT(out, i, r_empty_str);
    } else {
      SET_STRING_ELT(out, i, PRINTNAME(tag));
    }
  }

  FREE(1);
  return out;
}

static inline sexp* r_fn_eval_in_with_x_dots(sexp* fn, sexp* x, sexp* dots, sexp* env);
static inline sexp* r_c_eval_in_with_x_dots(sexp* x, sexp* dots, sexp* env);
static inline sexp* r_as_character(sexp* x, sexp* env);
static inline sexp* r_as_function(sexp* x, sexp* env);
static inline sexp* r_set_names_dispatch(sexp* x, sexp* nm, sexp* env);
static inline r_ssize r_length_dispatch(sexp* x, sexp* env);

sexp* rlang_set_names(sexp* x, sexp* mold, sexp* nm, sexp* env) {
  int n_kept = 0;

  sexp* dots = KEEP_N(rlang_dots(env), n_kept);

  if (!r_is_vector(x, -1)) {
    r_abort("`x` must be a vector");
  }

  if (nm == r_null) {
    x = r_set_names_dispatch(x, r_null, env);

    FREE(n_kept);
    return x;
  }

  if (r_is_function(nm) || r_is_formula(nm, -1, -1)) {
    if (r_is_null(r_names(mold))) {
      mold = KEEP_N(r_as_character(mold, env), n_kept);
    } else {
      mold = KEEP_N(rlang_names2(mold, env), n_kept);
    }

    nm = KEEP_N(r_as_function(nm, env), n_kept);
    nm = KEEP_N(r_fn_eval_in_with_x_dots(nm, mold, dots, env), n_kept);
  } else {
    if (r_length(dots) > 0) {
      nm = KEEP_N(r_c_eval_in_with_x_dots(nm, dots, env), n_kept);
    }

    nm = KEEP_N(r_as_character(nm, env), n_kept);
  }

  r_ssize n;
  if (r_is_object(x)) {
    n = r_length_dispatch(x, env);
  } else {
    n = r_length(x);
  }

  if (!r_is_character(nm, n)) {
    r_abort("`nm` must be `NULL` or a character vector the same length as `x`");
  }

  x = r_set_names_dispatch(x, nm, env);

  FREE(n_kept);
  return x;
}

static inline sexp* r_fn_eval_in_with_x_dots(sexp* fn, sexp* x, sexp* dots, sexp* env) {
  sexp* args = KEEP(r_new_node(r_syms_dot_x, dots));
  sexp* call = KEEP(r_new_call(r_syms_dot_fn, args));

  // This evaluates `fn(x, ...)`
  // `.x` is the first input, x
  // `.fn` is the function, fn
  // The dots are a pairlist already in the call
  sexp* out = r_eval_in_with_xy(call, env, x, r_syms_dot_x, fn, r_syms_dot_fn);
  FREE(2);
  return out;
}

static sexp* c_fn = NULL;
static inline sexp* r_c_eval_in_with_x_dots(sexp* x, sexp* dots, sexp* env) {
  return r_fn_eval_in_with_x_dots(c_fn, x, dots, env);
}

static sexp* as_character_call = NULL;
static inline sexp* r_as_character(sexp* x, sexp* env) {
  return r_eval_in_with_x(as_character_call, env, x, r_syms_dot_x);
}

static sexp* names_call = NULL;
static inline sexp* r_names_dispatch(sexp* x, sexp* env) {
  return r_eval_in_with_x(names_call, env, x, r_syms_dot_x);
}

extern sexp* rlang_ns_env;

// TODO: Replace with C implementation of `as_function()`
static sexp* as_function_call = NULL;
static inline sexp* r_as_function(sexp* x, sexp* env) {
  return r_eval_with_xy(as_function_call, rlang_ns_env, x, env);
}

// Use `names<-()` rather than setting names directly with `r_poke_names()`
// for genericity and for speed. `names<-()` can shallow duplicate `x`'s
// attributes using ALTREP wrappers, which is not in R's public API.
static sexp* set_names_call = NULL;
static inline sexp* r_set_names_dispatch(sexp* x, sexp* nm, sexp* env) {
  return r_eval_in_with_xy(set_names_call, env, x, r_syms_dot_x, nm, r_syms_dot_y);
}

static sexp* length_call = NULL;
static inline r_ssize r_length_dispatch(sexp* x, sexp* env) {
  sexp* n = KEEP(r_eval_in_with_x(length_call, env, x, r_syms_dot_x));

  if (r_length(n) != 1) {
    r_abort("Object length must have size 1, not %i", r_length(n));
  }

  r_ssize out;

  switch (r_typeof(n)) {
  case r_type_integer:
    out = (r_ssize) INTEGER(n)[0];
    break;
  case r_type_double:
    out = REAL(n)[0];
    break;
  default:
    r_abort("Object length has unknown type %s", r_type_as_c_string(r_typeof(n)));
  }

  FREE(1);
  return out;
}

void rlang_init_attr(sexp* ns) {
  c_fn = r_eval(r_sym("c"), r_base_env);

  as_character_call = r_parse("as.character(.x)");
  r_mark_precious(as_character_call);

  names_call = r_parse("names(.x)");
  r_mark_precious(names_call);

  as_function_call = r_parse("as_function(x, env = y)");
  r_mark_precious(as_function_call);

  set_names_call = r_parse("`names<-`(.x, .y)");
  r_mark_precious(set_names_call);

  length_call = r_parse("length(.x)");
  r_mark_precious(length_call);
}
