#include <rlang.h>
#include "internal.h"


static sexp* data_pronoun_names = NULL;
static sexp* data_pronoun_class = NULL;

// Exported for deprecated as_dictionary() generic
sexp* rlang_new_data_pronoun(sexp* x, sexp* lookup_msg, sexp* read_only) {
  sexp* dict = KEEP(r_new_vector(r_type_list, 3));

  r_list_poke(dict, 0, x);
  r_list_poke(dict, 2, read_only);

  if (lookup_msg == r_null) {
    r_list_poke(dict, 1, r_chr("Object `%s` not found in `.data`"));
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
    r_abort("`data` must be uniquely named but has duplicate columns");
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

  sexp* lookup_msg = KEEP_N(r_chr("Column `%s` not found in `.data`"), n_kept);
  sexp* read_only = KEEP_N(r_lgl(1), n_kept);
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
static void check_data_mask_top(sexp* bottom, sexp* top) {
  sexp* cur = bottom;

  while (cur != r_empty_env) {
    if (cur == top) {
      return ;
    }
    cur = r_env_parent(cur);
  }

  r_abort("Can't create data mask because `top` is not a parent of `bottom`");
}

static sexp* env_sym = NULL;
static sexp* old_sym = NULL;
static sexp* mask_sym = NULL;

static sexp* tilde_fn = NULL;
static sexp* restore_mask_fn = NULL;

static void on_exit_restore_lexical_env(sexp* mask, sexp* old, sexp* frame) {
  sexp* fn = r_duplicate(restore_mask_fn, true);

  sexp* env = r_new_environment(r_base_env, 2);
  r_env_poke(env, mask_sym, mask);
  r_env_poke(env, old_sym, old);
  r_fn_poke_env(fn, env);

  sexp* call = r_new_call(fn, r_null);
  r_on_exit(call, frame);
}

sexp* rlang_new_data_mask(sexp* bottom, sexp* top) {
  sexp* data_mask;

  if (bottom == r_null) {
    bottom = KEEP(r_new_environment(r_empty_env, 0));
    data_mask = bottom;
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
  if (top != bottom) {
    check_data_mask_top(bottom, top);
  }

  r_env_poke(data_mask, r_tilde_sym, tilde_fn);
  r_env_poke(data_mask, data_mask_flag_sym, data_mask);
  r_env_poke(data_mask, data_mask_env_sym, r_env_parent(top));
  r_env_poke(data_mask, data_mask_top_env_sym, top);

  FREE(1);
  return data_mask;
}


static bool is_data_mask(sexp* env) {
  return
    r_typeof(env) == r_type_environment &&
    r_env_has(env, data_mask_flag_sym);
}

static sexp* data_pronoun_sym = NULL;

static void warn_env_as_mask_once() {
  r_warn_deprecated_once("environment passed to rlang::as_data_mask()",
    "Passing an environment as data mask is deprecated.\n"
    "Please use `new_data_mask()` to transform your environment to a mask.\n"
    "\n"
    "  env <- env(foo = \"bar\")\n"
    "\n"
    "  # Bad:\n"
    "  as_data_mask(env)\n"
    "  eval_tidy(expr, env)\n"
    "\n"
    "  # Good:\n"
    "  mask <- new_data_mask(env)\n"
    "  eval_tidy(expr, mask)\n"
  );
}
sexp* rlang_as_data_mask(sexp* data) {
  if (is_data_mask(data)) {
    return data;
  }
  if (data == r_null) {
    return rlang_new_data_mask(r_null, r_null);
  }

  sexp* data_pronoun = rlang_as_data_pronoun(data);
  sexp* bottom = NULL;

  int n_protect = 0;

  switch (r_typeof(data)) {
  case r_type_environment:
    warn_env_as_mask_once();
    bottom = KEEP_N(r_env_clone(data, NULL), n_protect);
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
    bottom = KEEP_N(r_new_environment(r_empty_env, 0), n_protect);

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
  sexp* data_mask = rlang_new_data_mask(bottom, bottom);

  FREE(n_protect);
  return data_mask;
}

// For compatibility of the exported C callable
// TODO: warn
sexp* rlang_new_data_mask_compat(sexp* bottom, sexp* top, sexp* parent) {
  return rlang_new_data_mask(bottom, top);
}
sexp* rlang_as_data_mask_compat(sexp* data, sexp* parent) {
  return rlang_as_data_mask(data);
}


static sexp* tilde_prim = NULL;

static sexp* base_tilde_eval(sexp* tilde, sexp* quo_env) {
  if (r_f_has_env(tilde)) {
    return tilde;
  }

  // Inline the base primitive because overscopes override `~` to make
  // quosures self-evaluate
  tilde = KEEP(r_new_call(tilde_prim, r_node_cdr(tilde)));
  tilde = KEEP(r_eval(tilde, quo_env));

  // Change it back because the result still has the primitive inlined
  r_node_poke_car(tilde, r_tilde_sym);

  FREE(2);
  return tilde;
}


static sexp* env_poke_parent_fn = NULL;
static sexp* env_poke_fn = NULL;

sexp* rlang_tilde_eval(sexp* tilde, sexp* current_frame, sexp* caller_frame) {
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

  sexp* mask = r_env_find_anywhere(caller_frame, data_mask_flag_sym);
  if (mask == r_unbound_sym) {
    r_abort("Internal error: Can't find the data mask");
  }
  if (r_typeof(mask) != r_type_environment) {
    r_abort("Internal error: Unexpected type for data mask flag: `%s`",
            r_type_c_string(r_typeof(mask)));
  }

  sexp* top = r_env_find(mask, data_mask_top_env_sym);
  if (top == r_unbound_sym) {
    // Quosure mask case
    top = mask;
  } else {
    // Update `.env` pronoun to current quosure env temporarily
    r_env_poke(mask, data_mask_env_sym, quo_env);
  }

  // Unwind-protect the restoration of original parents
  on_exit_restore_lexical_env(mask, r_env_parent(top), current_frame);

  // Swap lexical contexts temporarily by rechaining the top of the
  // mask to the quosure environment
  r_env_poke_parent(top, quo_env);

  return r_eval(expr, mask);
}

static const char* data_mask_objects_names[5] = {
  ".__tidyeval_data_mask__.", "~", ".top_env", ".env", NULL
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
  r_env_unbind_all(mask, data_mask_objects_names, false);

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
  r_env_poke(mask, r_tilde_sym, tilde_fn);
  r_env_poke(mask, data_mask_flag_sym, mask);
  FREE(1);
  return mask;
}


sexp* rlang_eval_tidy(sexp* expr, sexp* data, sexp* frame) {
  int n_protect = 0;

  sexp* env;
  if (rlang_is_quosure(expr)) {
    env = r_quo_get_env(expr);
    expr = r_quo_get_expr(expr);
  } else {
    env = KEEP_N(r_eval(env_sym, frame), n_protect);
  }

  // If there is no data, we only need to mask `~` with the definition
  // for quosure thunks. Otherwise we create a heavier data mask with
  // all the masking objects, data pronouns, etc.
  if (data == r_null) {
    sexp* mask = KEEP_N(new_quosure_mask(env), n_protect);
    sexp* out = r_eval(expr, mask);
    FREE(n_protect);
    return out;
  }

  sexp* mask = KEEP_N(rlang_as_data_mask(data), n_protect);

  sexp* top = r_env_find(mask, data_mask_top_env_sym);
  if (top == r_unbound_sym) {
    r_abort("Internal error: Can't find .top pronoun in data mask");
  }
  if (r_typeof(top) != r_type_environment) {
    r_abort("Internal error: Unexpected .top pronoun type");
  }

  // Rechain the mask on the new lexical env but don't restore it on
  // exit. This way leaked masks inherit from a somewhat sensible
  // environment. We could do better with ALTENV and two-parent data
  // masks:
  //
  // * We'd create a new two-parents evaluation env for each quosure.
  //   The first parent would be the mask and the second the lexical
  //   environment.
  //
  // * The data mask top would always inherit from the empty
  //   environment.
  //
  // * Look-up in leaked environments would proceed from the data mask
  //   to the appropriate lexical environment (from quosures or from
  //   the `env` argument of eval_tidy()).
  r_env_poke(mask, data_mask_env_sym, env);
  r_env_poke_parent(top, env);

  sexp* out = r_eval(expr, mask);
  FREE(n_protect);
  return out;
}


const char* data_pronoun_c_names[4] = { "src", "lookup_msg", "read_only", NULL };

void rlang_init_eval_tidy() {
  tilde_fn = r_parse_eval(
    "function(...) {                          \n"
    "  .Call(rlang_tilde_eval,                \n"
    "    sys.call(),     # Quosure env        \n"
    "    environment(),  # Unwind-protect env \n"
    "    parent.frame()  # Lexical env        \n"
    "  )                                      \n"
    "}                                        \n",
    r_ns_env("rlang")
  );
  r_mark_precious(tilde_fn);

  data_pronoun_names = r_new_character(data_pronoun_c_names);
  r_mark_precious(data_pronoun_names);

  data_pronoun_class = r_chr("rlang_data_pronoun");
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

  env_sym = r_sym("env");
  old_sym = r_sym("old");
  mask_sym = r_sym("mask");

  restore_mask_fn = r_parse_eval(
    "function() {                \n"
    "  mask$.env <- `old`        \n"
    "                            \n"
    "  top <- `mask`$.top_env    \n"
    "  if (is.null(top)) {       \n"
    "    top <- `mask`           \n"
    "  }                         \n"
    "                            \n"
    "  parent.env(top) <- `old`  \n"
    "}                           \n",
    r_base_env
  );
  r_mark_precious(restore_mask_fn);
}
