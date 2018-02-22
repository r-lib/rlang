#include <rlang.h>
#include "internal.h"


static sexp* tilde_thunk_fmls = NULL;
static sexp* tilde_thunk_body = NULL;

sexp* new_tilde_thunk(sexp* data_mask, sexp* data_mask_top) {
  sexp* body = KEEP(r_duplicate(tilde_thunk_body, false));
  sexp* fn = KEEP(r_new_function(tilde_thunk_fmls, body, r_base_env));

  sexp* args = r_node_cdr(r_node_cddr(body));
  r_node_poke_car(args, data_mask);
  r_node_poke_cadr(args, data_mask_top);

  FREE(2);
  return fn;
}


static sexp* data_pronoun_names = NULL;
static sexp* data_pronoun_class = NULL;

// Exported for deprecated as_dictionary() generic
sexp* rlang_new_data_pronoun(sexp* x, sexp* lookup_msg, sexp* read_only) {
  sexp* dict = KEEP(r_new_vector(r_type_list, 3));

  r_list_poke(dict, 0, x);
  r_list_poke(dict, 2, read_only);

  if (lookup_msg == r_null) {
    r_list_poke(dict, 1, r_scalar_chr("Object `%s` not found in `.data`"));
  } else {
    r_list_poke(dict, 1, lookup_msg);
  }

  r_poke_attribute(dict, r_names_sym, data_pronoun_names);
  r_poke_attribute(dict, r_class_sym, data_pronoun_class);

  FREE(1);
  return dict;
}


static sexp* empty_names_chr;

static void check_unique_names(sexp* x) {
  // Allow empty lists
  if (!r_length(x)) {
    return ;
  }

  sexp* names = r_vec_names(x);
  if (names == r_null) {
    r_abort("`data` must be uniquely named but does not have names");
  }
  if (r_vec_find_first_duplicate(names, empty_names_chr, NULL)) {
    r_abort("`data` must be uniquely named but has duplicate elements");
  }
}
sexp* rlang_as_data_pronoun(sexp* x) {
  int n_kept = 0;

  switch (r_typeof(x)) {
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
    check_unique_names(x);
    x = KEEP_N(r_vec_coerce(x, r_type_list), n_kept);
    break;
  case r_type_list:
    check_unique_names(x);
    break;
  case r_type_environment:
    break;
  default:
    r_abort("`data` must be an uniquely named vector, list, data frame or environment");
  }

  sexp* lookup_msg = KEEP_N(r_scalar_chr("Column `%s` not found in `.data`"), n_kept);
  sexp* read_only = KEEP_N(r_scalar_lgl(1), n_kept);
  sexp* pronoun = rlang_new_data_pronoun(x, lookup_msg, read_only);

  FREE(n_kept);
  return pronoun;
}


static sexp* data_mask_flag_sym = NULL;
static sexp* data_mask_env_sym = NULL;
static sexp* data_mask_top_env_sym = NULL;

static void check_data_mask_input(sexp* env, const char* arg) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("Can't create data mask because `%s` must be an environment", arg);
  }
}
sexp* rlang_new_data_mask(sexp* bottom, sexp* top, sexp* parent) {
  check_data_mask_input(parent, "parent");
  sexp* data_mask;

  if (bottom == r_null) {
    data_mask = bottom = KEEP(r_new_environment(parent, 0));
  } else {
    check_data_mask_input(bottom, "bottom");
    // Create a child because we don't know what might be in `bottom`
    // and we need to clear its contents without deleting any object
    // created in the data mask environment
    data_mask = KEEP(r_new_environment(bottom, 0));
  }

  if (top == r_null) {
    top = bottom;
  } else {
    check_data_mask_input(top, "top");
  }

  r_env_poke(data_mask, r_tilde_sym, new_tilde_thunk(data_mask, top));
  r_env_poke(data_mask, data_mask_flag_sym, data_mask);
  r_env_poke(data_mask, data_mask_env_sym, parent);
  r_env_poke(data_mask, data_mask_top_env_sym, top);

  FREE(1);
  return data_mask;
}


static sexp* data_pronoun_sym = NULL;

sexp* rlang_as_data_mask(sexp* data, sexp* parent) {
  if (data == r_null) {
    return rlang_new_data_mask(r_null, r_null, parent);
  }
  sexp* data_pronoun = rlang_as_data_pronoun(data);
  sexp* bottom = NULL;

  int n_protect = 0;

  switch (r_typeof(data)) {
  case r_type_environment:
    bottom = KEEP_N(r_env_clone(data, parent), n_protect);
    break;

  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
    data = r_vec_coerce(data, r_type_list);
    KEEP_N(data, n_protect);
    // fallthrough:

  case r_type_list: {
    sexp* names = r_vec_names(data);
    bottom = KEEP_N(r_new_environment(parent, 0), n_protect);

    if (names != r_null) {
      r_ssize_t n = r_length(data);

      for (r_ssize_t i = 0; i < n; ++i) {
        // Ignore empty or missing names
        sexp* nm = r_chr_get(names, i);
        if (r_str_is_name(nm)) {
          sexp* elt = r_list_get(data, i);
          r_env_poke(bottom, r_str_as_symbol(nm), elt);
        }
      }
    }

    break;
  }

  default:
    r_abort("`data` must be a vector, list, data frame, or environment");
  }

  r_env_poke(bottom, data_pronoun_sym, data_pronoun);
  sexp* data_mask = rlang_new_data_mask(bottom, bottom, parent);

  FREE(n_protect);
  return data_mask;
}


static sexp* tilde_prim = NULL;

static sexp* base_tilde_eval(sexp* tilde, sexp* quo_env) {
  if (r_f_has_env(tilde)) {
    return tilde;
  }

  // Inline the base primitive because overscopes override `~` to make
  // quosures self-evaluate
  tilde = KEEP(r_new_call_node(tilde_prim, r_node_cdr(tilde)));
  tilde = KEEP(r_eval(tilde, quo_env));

  // Change it back because the result still has the primitive inlined
  r_node_poke_car(tilde, r_tilde_sym);

  FREE(2);
  return tilde;
}


static sexp* env_poke_parent_fn = NULL;
static sexp* env_poke_fn = NULL;

sexp* rlang_tilde_eval(sexp* tilde, sexp* overscope, sexp* overscope_top,
                       sexp* cur_frame, sexp* caller_frame) {
  // Remove srcrefs from system call
  r_poke_attribute(tilde, r_srcref_sym, r_null);

  if (!rlang_is_quosure(tilde)) {
    return base_tilde_eval(tilde, caller_frame);
  }
  if (quo_is_missing(tilde)) {
    return(r_missing_arg());
  }

  sexp* expr = rlang_quo_get_expr(tilde);
  if (!r_is_symbolic(expr)) {
    return expr;
  }

  sexp* quo_env = rlang_quo_get_env(tilde);
  if (r_typeof(quo_env) != r_type_environment) {
    r_abort("Internal error: Quosure environment is corrupt");
  }

  int n_protect = 0;

  sexp* prev_env;
  sexp* flag = r_env_find(overscope, data_mask_flag_sym);
  if (flag == r_unbound_sym) {
    prev_env = r_env_parent(overscope);
  } else {
    prev_env = r_env_get(overscope, data_mask_env_sym);
    KEEP_N(prev_env, n_protect); // Help rchk

    // Update .env pronoun to current quosure env temporarily
    r_env_poke(overscope, data_mask_env_sym, quo_env);

    sexp* exit_args = r_build_pairlist3(overscope, r_scalar_chr(".env"), prev_env);
    sexp* exit_lang = KEEP(r_build_call_node(env_poke_fn, exit_args));
    r_on_exit(exit_lang, cur_frame);
    FREE(1);
  }


  // Swap enclosures temporarily by rechaining the top of the dynamic
  // scope to the enclosure of the new formula, if it has one
  r_env_poke_parent(overscope_top, quo_env);

  sexp* exit_args = r_build_pairlist2(overscope_top, prev_env);
  sexp* exit_lang = r_build_call_node(env_poke_parent_fn, exit_args);
  KEEP_N(exit_lang, n_protect);
  r_on_exit(exit_lang, cur_frame);

  sexp* out = r_eval(expr, overscope);
  FREE(n_protect);
  return out;
}

#define DATA_MASK_OBJECTS_N 4
static const char* data_mask_objects_names[DATA_MASK_OBJECTS_N] = {
  ".__tidyeval_data_mask__.", "~", ".top_env", ".env"
};

// Soft-deprecated in rlang 0.2.0
sexp* rlang_data_mask_clean(sexp* mask) {
  sexp* bottom = r_env_parent(mask);
  sexp* top = r_env_get(mask, data_mask_top_env_sym);

  KEEP(top); // Help rchk

  if (top == r_null) {
    top = bottom;
  }

  // At this level we only want to remove our own stuff
  r_env_unbind_all(mask, data_mask_objects_names, DATA_MASK_OBJECTS_N, false);

  // Remove everything in the other levels
  sexp* env = bottom;
  sexp* parent = r_env_parent(top);
  while (env != parent) {
    r_env_unbind_names(env, r_env_names(env), false);
    env = r_env_parent(env);
  }

  FREE(1);
  return mask;
}


static sexp* new_quosure_mask(sexp* env) {
  sexp* mask = KEEP(r_new_environment(env, 3));
  r_env_poke(mask, r_tilde_sym, new_tilde_thunk(mask, mask));
  FREE(1);
  return mask;
}


bool is_data_mask(sexp* env) {
  if (r_typeof(env) != r_type_environment) {
    return false;
  } else {
    return r_env_find(env, data_mask_flag_sym) != r_unbound_sym;
  }
}


static sexp* data_mask_clean_fn = NULL;
static sexp* env_sym = NULL;

sexp* rlang_eval_tidy(sexp* expr, sexp* data, sexp* frame) {
  int n_protect = 0;

  sexp* env;
  if (rlang_is_quosure(expr)) {
    env = r_quo_get_env(expr);
    expr = r_quo_get_expr(expr);
  } else {
    env = KEEP_N(r_eval(env_sym, frame), n_protect);
  }

  // If `data` is already a data mask, update env pronouns and
  // evaluate in that environment. The caller is responsible for
  // cleaning the mask if needed.
  if (is_data_mask(data)) {
    r_env_poke(data, data_mask_env_sym, env);
    sexp* top = r_env_get(data, data_mask_top_env_sym);
    r_env_poke_parent(top, env);

    sexp* out = r_eval(expr, data);
    FREE(n_protect);
    return out;
  }


  sexp* mask;

  // If there is no data, we only need to mask `~` with the definition
  // for quosure thunks. Otherwise we create a heavier data mask with
  // all the masking objects, data pronouns, etc.
  if (data == r_null) {
    mask = new_quosure_mask(env);
  } else {
    mask = rlang_as_data_mask(data, env);
  }

  sexp* out = r_eval(expr, mask);
  FREE(n_protect);
  return out;
}


const char* data_pronoun_c_names[3] = { "src", "lookup_msg", "read_only" };

void rlang_init_eval_tidy() {
  tilde_thunk_fmls = rlang_constants_get("tilde_thunk_fmls");
  tilde_thunk_body = rlang_constants_get("tilde_thunk_body");

  data_pronoun_names = r_new_character(data_pronoun_c_names, 3);
  r_mark_precious(data_pronoun_names);

  data_pronoun_class = r_scalar_chr("rlang_data_pronoun");
  r_mark_precious(data_pronoun_class);

  empty_names_chr = r_new_vector(r_type_character, 2);
  r_mark_precious(empty_names_chr);
  r_chr_poke(empty_names_chr, 0, r_string(""));
  r_chr_poke(empty_names_chr, 1, r_missing_str);

  data_mask_flag_sym = r_sym(".__tidyeval_data_mask__.");
  data_mask_env_sym = r_sym(".env");
  data_mask_top_env_sym = r_sym(".top_env");
  data_pronoun_sym = r_sym(".data");

  tilde_prim = r_base_ns_get("~");
  env_poke_parent_fn = rlang_ns_get("env_poke_parent");
  env_poke_fn = rlang_ns_get("env_poke");

  data_mask_clean_fn = rlang_ns_get("overscope_clean");

  env_sym = r_sym("env");
}
