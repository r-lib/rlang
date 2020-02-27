#include <rlang.h>
#include "internal.h"

static inline sexp* r_node_names(sexp* x);
static inline sexp* r_names_dispatch(sexp* x);

sexp* rlang_names2(sexp* x) {
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
    nms = KEEP(r_names_dispatch(x));
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
  sexp** p_out = STRING_PTR(out);

  int i = 0;

  for(; x != r_null; x = r_node_cdr(x), ++i) {
    sexp* tag = r_node_tag(x);

    if (tag == r_null) {
      p_out[i] = r_empty_str;
    } else {
      p_out[i] = PRINTNAME(tag);
    }
  }

  FREE(1);
  r_mark_shared(out);
  return out;
}

static inline sexp* r_eval_fn_with_x_dots(sexp* fn, sexp* x, sexp* dots);
static inline sexp* r_eval_c_with_x_dots(sexp* x, sexp* dots);
static inline sexp* r_as_character(sexp* x);
static inline sexp* r_as_function(sexp* x);
static inline sexp* r_set_names_dispatch(sexp* x, sexp* nm);

sexp* rlang_set_names(sexp* call, sexp* op, sexp* args, sexp* env) {
  int n_kept = 0;

  args = r_node_cdr(args);

  sexp* x = KEEP_N(r_node_car(args), n_kept); args = r_node_cdr(args);
  sexp* mold = KEEP_N(r_node_car(args), n_kept); args = r_node_cdr(args);
  sexp* nm = KEEP_N(r_node_car(args), n_kept); args = r_node_cdr(args);

  sexp* dots = KEEP_N(rlang_dots(env), n_kept);

  if (!r_is_vector(x, -1)) {
    r_abort("`x` must be a vector");
  }

  if (nm == r_null) {
    x = r_set_names_dispatch(x, r_null);

    FREE(n_kept);
    return x;
  }

  if (r_is_function(nm) || r_is_formula(nm, -1, -1)) {
    if (r_is_null(r_names(mold))) {
      mold = KEEP_N(r_as_character(mold), n_kept);
    } else {
      mold = KEEP_N(rlang_names2(mold), n_kept);
    }

    nm = KEEP_N(r_as_function(nm), n_kept);
    nm = KEEP_N(r_eval_fn_with_x_dots(nm, mold, dots), n_kept);
  } else {
    if (r_length(dots) > 0) {
      nm = KEEP_N(r_eval_c_with_x_dots(nm, dots), n_kept);
    }

    nm = KEEP_N(r_as_character(nm), n_kept);
  }

  if (!r_is_character(nm, r_length(x))) {
    r_abort("`nm` must be `NULL` or a character vector the same length as `x`");
  }

  x = r_set_names_dispatch(x, nm);

  FREE(n_kept);
  return x;
}


static inline sexp* r_eval_fn_with_x(sexp* fn, sexp* x) {
  sexp* args = KEEP(r_new_node(x, r_null));

  sexp* call = KEEP(r_new_call(fn, args));

  sexp* out = r_eval(call, r_global_env);

  FREE(2);
  return out;
}

static inline sexp* r_eval_fn_with_x_y(sexp* fn, sexp* x, sexp* y) {
  sexp* args = KEEP(r_new_node(y, r_null));
  args = KEEP(r_new_node(x, args));

  sexp* call = KEEP(r_new_call(fn, args));

  sexp* out = r_eval(call, r_global_env);

  FREE(3);
  return out;
}

static inline sexp* r_eval_fn_with_x_dots(sexp* fn, sexp* x, sexp* dots) {
  sexp* args = KEEP(r_new_node(x, dots));
  sexp* call = KEEP(r_new_call(fn, args));

  sexp* out = r_eval(call, r_global_env);

  FREE(2);
  return out;
}

static sexp* c_fn = NULL;
static inline sexp* r_eval_c_with_x_dots(sexp* x, sexp* dots) {
  return r_eval_fn_with_x_dots(c_fn, x, dots);
}

static sexp* as_character_fn = NULL;
static inline sexp* r_as_character(sexp* x) {
  return r_eval_fn_with_x(as_character_fn, x);
}

static sexp* names_fn = NULL;
static inline sexp* r_names_dispatch(sexp* x) {
  return r_eval_fn_with_x(names_fn, x);
}

// TODO: Replace with C implementation of `as_function()`
static sexp* as_function_fn = NULL;
static inline sexp* r_as_function(sexp* x) {
  return r_eval_fn_with_x(as_function_fn, x);
}

// Use `names<-()` rather than setting names directly with `r_poke_names()`
// for genericity and for speed. `names<-()` can shallow duplicate `x`'s
// attributes using ALTREP wrappers, which is not in R's public API.
static sexp* set_names_fn = NULL;
static inline sexp* r_set_names_dispatch(sexp* x, sexp* nm) {
  return r_eval_fn_with_x_y(set_names_fn, x, nm);
}

void rlang_init_attr(sexp* ns) {
  c_fn = r_eval(r_sym("c"), r_base_env);
  as_character_fn = r_eval(r_sym("as.character"), r_base_env);
  names_fn = r_eval(r_sym("names"), r_base_env);
  as_function_fn = r_eval(r_sym("as_function"), ns);
  set_names_fn = r_eval(r_sym("names<-"), r_base_env);
}
