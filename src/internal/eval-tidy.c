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
    r_list_poke(dict, 1, r_scalar_chr("Object `%s` not found in data"));
  } else {
    r_list_poke(dict, 1, lookup_msg);
  }

  r_poke_attribute(dict, r_names_sym, data_pronoun_names);
  r_poke_attribute(dict, r_class_sym, data_pronoun_class);

  FREE(1);
  return dict;
}


static sexp* data_mask_sym = NULL;
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
    data_mask = bottom = KEEP(r_new_environment(parent));
  } else {
    check_data_mask_input(bottom, "bottom");
    // Create a child because we don't know what might be in `bottom`
    // and we need to clear its contents without deleting any object
    // created in the data mask environment
    data_mask = KEEP(r_new_environment(bottom));
  }

  if (top == r_null) {
    top = bottom;
  } else {
    check_data_mask_input(top, "top");
  }

  r_env_poke(data_mask, r_tilde_sym, new_tilde_thunk(data_mask, top));
  r_env_poke(data_mask, data_mask_sym, data_mask);
  r_env_poke(data_mask, data_mask_env_sym, parent);
  r_env_poke(data_mask, data_mask_top_env_sym, top);

  FREE(1);
  return data_mask;
}


static sexp* data_pronoun_sym = NULL;

sexp* rlang_as_data_mask(sexp* data, sexp* data_src, sexp* parent) {
  if (data == r_null) {
    return rlang_new_data_mask(r_null, r_null, parent);
  }

  sexp* bottom = NULL;

  switch (r_typeof(data)) {
  case r_type_environment:
    bottom = KEEP(r_env_clone(data, parent));
    break;

  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
    data = r_vec_coerce(data, r_type_list);
    // fallthrough;
  case r_type_list: {
    sexp* names = r_names(data);
    bottom = KEEP(r_new_environment(parent));

    if (names != r_null) {
      r_size_t n = r_length(data);

      for (r_size_t i = 0; i < n; ++i) {
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
    r_abort("`data` must be a list, vector, or environment");
  }

  r_env_poke(bottom, data_pronoun_sym, data_src);
  sexp* data_mask = rlang_new_data_mask(bottom, bottom, parent);

  FREE(1);
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
static sexp* env_set_fn = NULL;

sexp* rlang_tilde_eval(sexp* tilde, sexp* overscope, sexp* overscope_top, sexp* cur_frame) {
  if (!rlang_is_quosure(tilde)) {
    return base_tilde_eval(tilde, overscope);
  }
  if (quo_is_missing(tilde)) {
    return(r_missing_arg());
  }

  sexp* quo_env = r_f_env(tilde);
  sexp* prev_env = r_env_get(overscope, r_sym(".env"));
  if (r_is_null(quo_env)) {
    quo_env = prev_env;
  }

  // Swap enclosures temporarily by rechaining the top of the dynamic
  // scope to the enclosure of the new formula, if it has one
  r_env_poke_parent(overscope_top, quo_env);

  sexp* exit_args = r_build_pairlist2(overscope_top, prev_env);
  sexp* exit_lang = KEEP(r_build_call_node(env_poke_parent_fn, exit_args));
  r_on_exit(exit_lang, cur_frame);
  FREE(1);

  // Update .env pronoun to current quosure env temporarily
  r_env_poke(overscope, data_mask_env_sym, quo_env);

  exit_args = r_build_pairlist3(overscope, r_scalar_chr(".env"), prev_env);
  exit_lang = KEEP(r_build_call_node(env_set_fn, exit_args));
  r_on_exit(exit_lang, cur_frame);
  FREE(1);

  return r_eval(r_f_rhs(tilde), overscope);
}

#define DATA_MASK_OBJECTS_N 3
static const char* data_mask_objects_names[DATA_MASK_OBJECTS_N] = {
   "~", ".top_env", ".env"
};

sexp* rlang_data_mask_clean(sexp* mask) {
  sexp* bottom = r_env_parent(mask);
  sexp* top = r_env_get(mask, data_mask_top_env_sym);

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

  return mask;
}


const char* data_pronoun_c_names[3] = { "src", "lookup_msg", "read_only" };

void rlang_init_eval_tidy() {
  tilde_thunk_fmls = rlang_constants_get("tilde_thunk_fmls");
  tilde_thunk_body = rlang_constants_get("tilde_thunk_body");

  data_pronoun_names = r_new_character(data_pronoun_c_names, 3);
  r_mark_precious(data_pronoun_names);

  data_pronoun_class = r_scalar_chr("dictionary");
  r_mark_precious(data_pronoun_class);

  data_mask_sym = r_sym(".__tidyeval_data_mask__.");
  data_mask_env_sym = r_sym(".env");
  data_mask_top_env_sym = r_sym(".top_env");
  data_pronoun_sym = r_sym(".data");

  tilde_prim = r_base_ns_get("~");
  env_poke_parent_fn = rlang_ns_get("env_poke_parent");
  env_set_fn = rlang_ns_get("env_set");
}
