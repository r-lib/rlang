#include <rlang.h>
#include "internal.h"


static r_obj* quo_mask_flag_sym = NULL;
static r_obj* data_mask_flag_sym = NULL;

enum rlang_mask_type {
  RLANG_MASK_DATA,     // Full data mask
  RLANG_MASK_QUOSURE,  // Quosure mask with only `~` binding
  RLANG_MASK_NONE
};

struct rlang_mask_info {
  r_obj* mask;
  enum rlang_mask_type type;
};

static struct rlang_mask_info mask_info(r_obj* mask) {
  if (r_typeof(mask) != R_TYPE_environment) {
    return (struct rlang_mask_info) { r_null, RLANG_MASK_NONE };
  }

  r_obj* flag;

  flag = r_env_find_anywhere(mask, data_mask_flag_sym);
  if (flag != r_syms.unbound) {
    return (struct rlang_mask_info) { flag, RLANG_MASK_DATA };
  }

  flag = r_env_find_anywhere(mask, quo_mask_flag_sym);
  if (flag != r_syms.unbound) {
    return (struct rlang_mask_info) { flag, RLANG_MASK_QUOSURE };
  }

  return (struct rlang_mask_info) { r_null, RLANG_MASK_NONE };
}


static r_obj* data_pronoun_class = NULL;
static r_obj* ctxt_pronoun_class = NULL;
static r_obj* data_mask_env_sym = NULL;

static r_obj* rlang_new_data_pronoun(r_obj* mask) {
  r_obj* pronoun = KEEP(r_alloc_list(1));

  r_list_poke(pronoun, 0, mask);
  r_attrib_poke(pronoun, r_syms.class_, data_pronoun_class);

  FREE(1);
  return pronoun;
}
static r_obj* rlang_new_ctxt_pronoun(r_obj* top) {
  r_obj* pronoun = KEEP(r_alloc_empty_environment(r_env_parent(top)));

  r_attrib_poke(pronoun, r_syms.class_, ctxt_pronoun_class);

  FREE(1);
  return pronoun;
}

void poke_ctxt_env(r_obj* mask, r_obj* env) {
  r_obj* ctxt_pronoun = r_env_find(mask, data_mask_env_sym);

  if (ctxt_pronoun == r_syms.unbound) {
    r_abort("Internal error: Can't find context pronoun in data mask");
  }

  r_env_poke_parent(ctxt_pronoun, env);
}


static r_obj* empty_names_chr;

static void check_unique_names(r_obj* x) {
  // Allow empty lists
  if (!r_length(x)) {
    return ;
  }

  r_obj* names = r_names(x);
  if (names == r_null) {
    r_abort("`data` must be uniquely named but does not have names");
  }
  if (vec_find_first_duplicate(names, empty_names_chr, NULL)) {
    r_abort("`data` must be uniquely named but has duplicate columns");
  }
}
r_obj* ffi_as_data_pronoun(r_obj* x) {
  int n_kept = 0;

  switch (r_typeof(x)) {
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_character:
  case R_TYPE_raw:
    x = KEEP_N(r_vec_coerce(x, R_TYPE_list), &n_kept);
    // fallthrough
  case R_TYPE_list:
    check_unique_names(x);
    x = KEEP_N(r_list_as_environment(x, r_envs.empty), &n_kept);
    break;
  case R_TYPE_environment:
    break;
  default:
    r_abort("`data` must be an uniquely named vector, list, data frame or environment");
  }

  r_obj* pronoun = rlang_new_data_pronoun(x);

  FREE(n_kept);
  return pronoun;
}


static r_obj* data_mask_top_env_sym = NULL;

static void check_data_mask_input(r_obj* env, const char* arg) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("Can't create data mask because `%s` must be an environment", arg);
  }
}
static void check_data_mask_top(r_obj* bottom, r_obj* top) {
  r_obj* cur = bottom;

  while (cur != r_envs.empty) {
    if (cur == top) {
      return ;
    }
    cur = r_env_parent(cur);
  }

  r_abort("Can't create data mask because `top` is not a parent of `bottom`");
}

static r_obj* env_sym = NULL;
static r_obj* old_sym = NULL;
static r_obj* mask_sym = NULL;

static r_obj* tilde_fn = NULL;
static r_obj* restore_mask_fn = NULL;

static void on_exit_restore_lexical_env(r_obj* mask, r_obj* old, r_obj* frame) {
  r_obj* fn = KEEP(r_clone(restore_mask_fn));

  r_obj* env = KEEP(r_alloc_environment(2, r_envs.base));
  r_env_poke(env, mask_sym, mask);
  r_env_poke(env, old_sym, old);
  r_fn_poke_env(fn, env);

  r_obj* call = KEEP(r_new_call(fn, r_null));
  r_on_exit(call, frame);

  FREE(3);
}

r_obj* ffi_new_data_mask(r_obj* bottom, r_obj* top) {
  r_obj* data_mask;

  if (bottom == r_null) {
    bottom = KEEP(r_alloc_environment(10, r_envs.empty));
    data_mask = bottom;
  } else {
    check_data_mask_input(bottom, "bottom");
    // Create a child because we don't know what might be in `bottom`
    // and we need to clear its contents without deleting any object
    // created in the data mask environment
    data_mask = KEEP(r_alloc_environment(10, bottom));
  }

  if (top == r_null) {
    top = bottom;
  } else {
    check_data_mask_input(top, "top");
  }
  if (top != bottom) {
    check_data_mask_top(bottom, top);
  }

  r_obj* ctxt_pronoun = KEEP(rlang_new_ctxt_pronoun(top));

  r_env_poke(data_mask, r_syms.tilde, tilde_fn);
  r_env_poke(data_mask, data_mask_flag_sym, data_mask);
  r_env_poke(data_mask, data_mask_env_sym, ctxt_pronoun);
  r_env_poke(data_mask, data_mask_top_env_sym, top);

  FREE(2);
  return data_mask;
}


r_obj* ffi_is_data_mask(r_obj* env) {
  return r_lgl(mask_info(env).type == RLANG_MASK_DATA);
}

static r_obj* mask_find(r_obj* env, r_obj* sym) {
  if (r_typeof(sym) != R_TYPE_symbol) {
    r_abort("Internal error: Data pronoun must be subset with a symbol");
  }

  r_obj* top_env = r_env_find(env, data_mask_top_env_sym);
  if (r_typeof(top_env) == R_TYPE_environment) {
    // Start lookup in the parent if the pronoun wraps a data mask
    env = r_env_parent(env);
  } else {
    // Data pronouns created from lists or data frames are converted
    // to a simple environment whose ancestry shouldn't be looked up.
    top_env = env;
  }
  int n_kept = 0;
  KEEP_N(top_env, &n_kept);

  r_obj* cur = env;
  do {
    r_obj* obj = r_env_find(cur, sym);
    if (r_typeof(obj) == R_TYPE_promise) {
      KEEP(obj);
      obj = r_eval(obj, r_envs.empty);
      FREE(1);
    }

    if (obj != r_syms.unbound) {
      FREE(n_kept);
      return obj;
    }

    if (cur == top_env) {
      break;
    } else {
      cur = r_env_parent(cur);
    }
  } while (cur != r_envs.empty);

  FREE(n_kept);
  return r_syms.unbound;
}
r_obj* ffi_data_pronoun_get(r_obj* pronoun, r_obj* sym, r_obj* error_call) {
  if (r_typeof(pronoun) != R_TYPE_environment) {
    r_abort("Internal error: Data pronoun must wrap an environment");
  }

  r_obj* obj = mask_find(pronoun, sym);

  if (obj == r_syms.unbound) {
    r_obj* call = KEEP(r_parse("abort_data_pronoun(x, call = y)"));
    r_eval_with_xy(call, sym, error_call, rlang_ns_env);
    r_abort("Internal error: .data subsetting should have failed earlier");
  }

  r_mark_shared(obj);
  return obj;
}

static void warn_env_as_mask_once(void) {
  const char* msg =
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
    "  eval_tidy(expr, mask)";
  deprecate_warn(msg, msg);
}

static r_obj* data_pronoun_sym = NULL;
static r_ssize mask_length(r_ssize n);

r_obj* ffi_as_data_mask(r_obj* data) {
  if (mask_info(data).type == RLANG_MASK_DATA) {
    return data;
  }
  if (data == r_null) {
    return ffi_new_data_mask(r_null, r_null);
  }

  int n_kept = 0;

  r_obj* bottom = NULL;

  switch (r_typeof(data)) {
  case R_TYPE_environment:
    warn_env_as_mask_once();
    bottom = KEEP_N(r_env_clone(data, NULL), &n_kept);
    break;

  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_character:
  case R_TYPE_raw:
    data = r_vec_coerce(data, R_TYPE_list);
    KEEP_N(data, &n_kept);
    // fallthrough:

  case R_TYPE_list: {
    check_unique_names(data);

    r_obj* names = r_names(data);

    r_ssize n_mask = mask_length(r_length(data));
    bottom = KEEP_N(r_alloc_environment(n_mask, r_envs.empty), &n_kept);

    if (names != r_null) {
      r_ssize n = r_length(data);

      r_obj* const * p_names = r_chr_cbegin(names);
      r_obj* const * p_data = r_list_cbegin(data);

      for (r_ssize i = 0; i < n; ++i) {
        // Ignore empty or missing names
        r_obj* nm = p_names[i];
        if (r_str_is_name(nm)) {
          r_env_poke(bottom, r_str_as_symbol(nm), p_data[i]);
        }
      }
    }

    break;
  }

  default:
    r_abort("`data` must be a vector, list, data frame, or environment");
  }

  r_obj* data_mask = KEEP_N(ffi_new_data_mask(bottom, bottom), &n_kept);

  r_obj* data_pronoun = KEEP_N(ffi_as_data_pronoun(data_mask), &n_kept);
  r_env_poke(bottom, data_pronoun_sym, data_pronoun);

  FREE(n_kept);
  return data_mask;
}

static
r_ssize mask_length(r_ssize n) {
  r_ssize n_grown = r_double_as_ssize(r_double_mult(r_ssize_as_double(n), 1.05));
  return r_ssize_max(n_grown, r_ssize_add(n, 20));
}

// For compatibility of the exported C callable
// TODO: warn
r_obj* ffi_new_data_mask_compat(r_obj* bottom, r_obj* top, r_obj* parent) {
  return ffi_new_data_mask(bottom, top);
}
r_obj* ffi_as_data_mask_compat(r_obj* data, r_obj* parent) {
  return ffi_as_data_mask(data);
}


static r_obj* tilde_prim = NULL;

static r_obj* base_tilde_eval(r_obj* tilde, r_obj* quo_env) {
  if (r_f_has_env(tilde)) {
    return tilde;
  }

  // Inline the base primitive because overscopes override `~` to make
  // quosures self-evaluate
  tilde = KEEP(r_new_call(tilde_prim, r_node_cdr(tilde)));
  tilde = KEEP(r_eval(tilde, quo_env));

  // Change it back because the result still has the primitive inlined
  r_node_poke_car(tilde, r_syms.tilde);

  FREE(2);
  return tilde;
}

r_obj* env_get_top_binding(r_obj* mask) {
  r_obj* top = r_env_find(mask, data_mask_top_env_sym);

  if (top == r_syms.unbound) {
    r_abort("Internal error: Can't find .top pronoun in data mask");
  }
  if (r_typeof(top) != R_TYPE_environment) {
    r_abort("Internal error: Unexpected .top pronoun type");
  }

  return top;
}


static r_obj* env_poke_parent_fn = NULL;
static r_obj* env_poke_fn = NULL;

r_obj* tilde_eval(r_obj* tilde, r_obj* current_frame, r_obj* caller_frame) {
  // Remove srcrefs from system call
  r_attrib_poke(tilde, r_syms.srcref, r_null);

  if (!is_quosure(tilde)) {
    return base_tilde_eval(tilde, caller_frame);
  }
  if (quo_is_missing(tilde)) {
    return(r_missing_arg);
  }

  r_obj* expr = quo_get_expr(tilde);
  if (!r_is_symbolic(expr)) {
    return expr;
  }

  r_obj* quo_env = ffi_quo_get_env(tilde);
  if (r_typeof(quo_env) != R_TYPE_environment) {
    r_abort("Internal error: Quosure environment is corrupt");
  }

  int n_kept = 0;
  r_obj* top = r_null;
  struct rlang_mask_info info = mask_info(caller_frame);

  switch (info.type) {
  case RLANG_MASK_DATA:
    top = KEEP_N(env_get_top_binding(info.mask), &n_kept);
    // Update `.env` pronoun to current quosure env temporarily
    poke_ctxt_env(info.mask, quo_env);
    break;
  case RLANG_MASK_QUOSURE:
    top = info.mask;
    break;
  case RLANG_MASK_NONE:
    r_abort("Internal error: Can't find the data mask");
  }

  // Unless the quosure was created in the mask, swap lexical contexts
  // temporarily by rechaining the top of the mask to the quosure
  // environment
  if (!r_env_inherits(quo_env, info.mask, top)) {
    // Unwind-protect the restoration of original parents
    on_exit_restore_lexical_env(info.mask, r_env_parent(top), current_frame);
    r_env_poke_parent(top, quo_env);
  }

  FREE(n_kept);
  return r_eval(expr, info.mask);
}

r_obj* ffi_tilde_eval(r_obj* call, r_obj* op, r_obj* args, r_obj* rho) {
  args = r_node_cdr(args);
  r_obj* tilde = r_node_car(args); args = r_node_cdr(args);
  r_obj* current_frame = r_node_car(args); args = r_node_cdr(args);
  r_obj* caller_frame = r_node_car(args);
  return tilde_eval(tilde, current_frame, caller_frame);
}

static const char* data_mask_objects_names[4] = {
  ".__tidyeval_data_mask__.", "~", ".top_env", ".env"
};

// Soft-deprecated in rlang 0.2.0
r_obj* ffi_data_mask_clean(r_obj* mask) {
  r_obj* bottom = r_env_parent(mask);
  r_obj* top = r_eval(data_mask_top_env_sym, mask);

  KEEP(top); // Help rchk

  if (top == r_null) {
    top = bottom;
  }

  // At this level we only want to remove our own stuff
  r_env_unbind_c_strings(mask,
                         data_mask_objects_names,
                         R_ARR_SIZEOF(data_mask_objects_names));

  // Remove everything in the other levels
  r_obj* env = bottom;
  r_obj* parent = r_env_parent(top);
  while (env != parent) {
    r_obj* nms = KEEP(r_env_names(env));
    r_env_unbind_names(env, nms);
    FREE(1);
    env = r_env_parent(env);
  }

  FREE(1);
  return mask;
}


static r_obj* new_quosure_mask(r_obj* env) {
  r_obj* mask = KEEP(r_alloc_environment(3, env));
  r_env_poke(mask, r_syms.tilde, tilde_fn);
  r_env_poke(mask, quo_mask_flag_sym, mask);
  FREE(1);
  return mask;
}

r_obj* rlang_eval_tidy(r_obj* expr, r_obj* data, r_obj* env) {
  int n_kept = 0;

  if (is_quosure(expr)) {
    env = r_quo_get_env(expr);
    expr = r_quo_get_expr(expr);
  }

  // If there is no data, we only need to mask `~` with the definition
  // for quosure thunks. Otherwise we create a heavier data mask with
  // all the masking objects, data pronouns, etc.
  if (data == r_null) {
    r_obj* mask = KEEP_N(new_quosure_mask(env), &n_kept);
    r_obj* out = r_eval(expr, mask);
    FREE(n_kept);
    return out;
  }

  r_obj* mask = KEEP_N(ffi_as_data_mask(data), &n_kept);
  r_obj* top = KEEP_N(env_get_top_binding(mask), &n_kept);

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
  if (!r_env_inherits(env, mask, top)) {
    poke_ctxt_env(mask, env);
    r_env_poke_parent(top, env);
  }

  r_obj* out = r_eval(expr, mask);
  FREE(n_kept);
  return out;
}

r_obj* ffi_eval_tidy(r_obj* call, r_obj* op, r_obj* args, r_obj* rho) {
  args = r_node_cdr(args);
  r_obj* expr = r_node_car(args); args = r_node_cdr(args);
  r_obj* data = r_node_car(args); args = r_node_cdr(args);
  r_obj* env = r_node_car(args);
  return rlang_eval_tidy(expr, data, env);
}


void rlang_init_eval_tidy(void) {
  r_obj* rlang_ns_env = KEEP(r_ns_env("rlang"));

  tilde_fn = r_eval(r_sym("tilde_eval"), rlang_ns_env);

  data_pronoun_class = r_chr("rlang_data_pronoun");
  r_preserve(data_pronoun_class);

  ctxt_pronoun_class = r_chr("rlang_ctxt_pronoun");
  r_preserve(ctxt_pronoun_class);

  empty_names_chr = r_alloc_character(2);
  r_preserve(empty_names_chr);
  r_chr_poke(empty_names_chr, 0, r_str(""));
  r_chr_poke(empty_names_chr, 1, r_globals.na_str);

  quo_mask_flag_sym = r_sym(".__tidyeval_quosure_mask__.");
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
    "function() {                          \n"
    "  ctxt_pronoun <- `mask`$.env         \n"
    "  if (!is.null(ctxt_pronoun)) {       \n"
    "    parent.env(ctxt_pronoun) <- `old` \n"
    "  }                                   \n"
    "                                      \n"
    "  top <- `mask`$.top_env              \n"
    "  if (is.null(top)) {                 \n"
    "    top <- `mask`                     \n"
    "  }                                   \n"
    "                                      \n"
    "  parent.env(top) <- `old`            \n"
    "}                                     \n",
    r_envs.base
  );
  r_preserve(restore_mask_fn);

  FREE(1);
}
