#include <rlang.h>
#include "internal.h"


static sexp* quo_mask_flag_sym = NULL;
static sexp* data_mask_flag_sym = NULL;

enum rlang_mask_type {
  RLANG_MASK_DATA,     // Full data mask
  RLANG_MASK_QUOSURE,  // Quosure mask with only `~` binding
  RLANG_MASK_NONE
};

struct rlang_mask_info {
  sexp* mask;
  enum rlang_mask_type type;
};

static struct rlang_mask_info mask_info(sexp* mask) {
  if (r_typeof(mask) != r_type_environment) {
    return (struct rlang_mask_info) { r_null, RLANG_MASK_NONE };
  }

  sexp* flag;

  flag = r_env_find_anywhere(mask, data_mask_flag_sym);
  if (flag != r_unbound_sym) {
    return (struct rlang_mask_info) { flag, RLANG_MASK_DATA };
  }

  flag = r_env_find_anywhere(mask, quo_mask_flag_sym);
  if (flag != r_unbound_sym) {
    return (struct rlang_mask_info) { flag, RLANG_MASK_QUOSURE };
  }

  return (struct rlang_mask_info) { r_null, RLANG_MASK_NONE };
}


static sexp* data_pronoun_class = NULL;
static sexp* ctxt_pronoun_class = NULL;
static sexp* data_mask_env_sym = NULL;

static sexp* rlang_new_data_pronoun(sexp* mask) {
  sexp* pronoun = KEEP(r_new_vector(r_type_list, 1));

  r_list_poke(pronoun, 0, mask);
  r_poke_attribute(pronoun, r_class_sym, data_pronoun_class);

  FREE(1);
  return pronoun;
}
static sexp* rlang_new_ctxt_pronoun(sexp* top) {
  sexp* pronoun = KEEP(r_new_environment(r_env_parent(top), 0));

  r_poke_attribute(pronoun, r_class_sym, ctxt_pronoun_class);

  FREE(1);
  return pronoun;
}

void poke_ctxt_env(sexp* mask, sexp* env) {
  sexp* ctxt_pronoun = r_env_find(mask, data_mask_env_sym);

  if (ctxt_pronoun == r_unbound_sym) {
    r_abort("Internal error: Can't find context pronoun in data mask");
  }

  r_env_poke_parent(ctxt_pronoun, env);
}


static sexp* empty_names_chr;

static void check_unique_names(sexp* x) {
  // Allow empty lists
  if (!r_length(x)) {
    return ;
  }

  sexp* names = r_names(x);
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
    x = KEEP_N(r_vec_coerce(x, r_type_list), n_kept);
    // fallthrough
  case r_type_list:
    check_unique_names(x);
    x = KEEP_N(r_list_as_environment(x, r_empty_env), n_kept);
    break;
  case r_type_environment:
    break;
  default:
    r_abort("`data` must be an uniquely named vector, list, data frame or environment");
  }

  sexp* pronoun = rlang_new_data_pronoun(x);

  FREE(n_kept);
  return pronoun;
}


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
  sexp* fn = KEEP(r_duplicate(restore_mask_fn, true));

  sexp* env = KEEP(r_new_environment(r_base_env, 2));
  r_env_poke(env, mask_sym, mask);
  r_env_poke(env, old_sym, old);
  r_fn_poke_env(fn, env);

  sexp* call = KEEP(r_new_call(fn, r_null));
  r_on_exit(call, frame);

  FREE(3);
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

  sexp* ctxt_pronoun = KEEP(rlang_new_ctxt_pronoun(top));

  r_env_poke(data_mask, r_tilde_sym, tilde_fn);
  r_env_poke(data_mask, data_mask_flag_sym, data_mask);
  r_env_poke(data_mask, data_mask_env_sym, ctxt_pronoun);
  r_env_poke(data_mask, data_mask_top_env_sym, top);

  FREE(2);
  return data_mask;
}


sexp* rlang_is_data_mask(sexp* env) {
  return r_lgl(mask_info(env).type == RLANG_MASK_DATA);
}

static sexp* mask_find(sexp* env, sexp* sym) {
  if (r_typeof(sym) != r_type_symbol) {
    r_abort("Internal error: Data pronoun must be subset with a symbol");
  }

  sexp* top_env = r_env_find(env, data_mask_top_env_sym);
  if (r_typeof(top_env) == r_type_environment) {
    // Start lookup in the parent if the pronoun wraps a data mask
    env = r_env_parent(env);
  } else {
    // Data pronouns created from lists or data frames are converted
    // to a simple environment whose ancestry shouldn't be looked up.
    top_env = env;
  }
  int n_kept = 0;
  KEEP_N(top_env, n_kept); // Help rchk

  sexp* cur = env;
  do {
    sexp* obj = r_env_find(cur, sym);
    if (TYPEOF(obj) == PROMSXP) {
      obj = KEEP_N(r_eval(obj, r_empty_env), n_kept);
    }
    if (obj != r_unbound_sym && !r_is_function(obj)) {
      FREE(n_kept);
      return obj;
    }

    if (cur == top_env) {
      break;
    } else {
      cur = r_env_parent(cur);
    }
  } while (cur != r_empty_env);

  FREE(n_kept);
  return r_unbound_sym;
}
sexp* rlang_data_pronoun_get(sexp* pronoun, sexp* sym) {
  if (r_typeof(pronoun) != r_type_environment) {
    r_abort("Internal error: Data pronoun must wrap an environment");
  }

  sexp* obj = mask_find(pronoun, sym);

  if (obj == r_unbound_sym) {
    sexp* call = KEEP(r_parse("rlang:::abort_data_pronoun(x)"));
    r_eval_with_x(call, r_base_env, sym);
    r_abort("Internal error: .data subsetting should have failed earlier");
  }

  r_mark_shared(obj);
  return obj;
}

static void warn_env_as_mask_once() {
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
  r_warn_deprecated(msg, msg);
}

static sexp* data_pronoun_sym = NULL;

sexp* rlang_as_data_mask(sexp* data) {
  if (mask_info(data).type == RLANG_MASK_DATA) {
    return data;
  }
  if (data == r_null) {
    return rlang_new_data_mask(r_null, r_null);
  }

  int n_protect = 0;

  sexp* bottom = NULL;

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
    check_unique_names(data);

    sexp* names = r_names(data);
    bottom = KEEP_N(r_new_environment(r_empty_env, 0), n_protect);

    if (names != r_null) {
      r_ssize n = r_length(data);

      for (r_ssize i = 0; i < n; ++i) {
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

  sexp* data_mask = KEEP_N(rlang_new_data_mask(bottom, bottom), n_protect);

  sexp* data_pronoun = KEEP_N(rlang_as_data_pronoun(data_mask), n_protect);
  r_env_poke(bottom, data_pronoun_sym, data_pronoun);

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

sexp* env_get_top_binding(sexp* mask) {
  sexp* top = r_env_find(mask, data_mask_top_env_sym);

  if (top == r_unbound_sym) {
    r_abort("Internal error: Can't find .top pronoun in data mask");
  }
  if (r_typeof(top) != r_type_environment) {
    r_abort("Internal error: Unexpected .top pronoun type");
  }

  return top;
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

  int n_protect = 0;
  sexp* top;
  struct rlang_mask_info info = mask_info(caller_frame);

  switch (info.type) {
  case RLANG_MASK_DATA:
    top = KEEP_N(env_get_top_binding(info.mask), n_protect);
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
  if (!r_env_inherits(info.mask, quo_env, top)) {
    // Unwind-protect the restoration of original parents
    on_exit_restore_lexical_env(info.mask, r_env_parent(top), current_frame);
    r_env_poke_parent(top, quo_env);
  }

  FREE(n_protect);
  return r_eval(expr, info.mask);
}

static const char* data_mask_objects_names[5] = {
  ".__tidyeval_data_mask__.", "~", ".top_env", ".env", NULL
};

// Soft-deprecated in rlang 0.2.0
sexp* rlang_data_mask_clean(sexp* mask) {
  sexp* bottom = r_env_parent(mask);
  sexp* top = r_eval(data_mask_top_env_sym, mask);

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
    sexp* nms = KEEP(r_env_names(env));
    r_env_unbind_names(env, nms, false);
    FREE(1);
    env = r_env_parent(env);
  }

  FREE(1);
  return mask;
}


static sexp* new_quosure_mask(sexp* env) {
  sexp* mask = KEEP(r_new_environment(env, 3));
  r_env_poke(mask, r_tilde_sym, tilde_fn);
  r_env_poke(mask, quo_mask_flag_sym, mask);
  FREE(1);
  return mask;
}

sexp* rlang_eval_tidy(sexp* expr, sexp* data, sexp* env) {
  int n_protect = 0;

  if (rlang_is_quosure(expr)) {
    env = r_quo_get_env(expr);
    expr = r_quo_get_expr(expr);
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
  sexp* top = KEEP_N(env_get_top_binding(mask), n_protect);

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
  if (!r_env_inherits(mask, env, top)) {
    poke_ctxt_env(mask, env);
    r_env_poke_parent(top, env);
  }

  sexp* out = r_eval(expr, mask);
  FREE(n_protect);
  return out;
}


void rlang_init_eval_tidy() {
  sexp* rlang_ns_env = KEEP(r_ns_env("rlang"));

  tilde_fn = r_parse_eval(
    "function(...) {                          \n"
    "  .Call(rlang_tilde_eval,                \n"
    "    sys.call(),     # Quosure env        \n"
    "    environment(),  # Unwind-protect env \n"
    "    parent.frame()  # Lexical env        \n"
    "  )                                      \n"
    "}                                        \n",
    rlang_ns_env
  );
  r_mark_precious(tilde_fn);

  data_pronoun_class = r_chr("rlang_data_pronoun");
  r_mark_precious(data_pronoun_class);

  ctxt_pronoun_class = r_chr("rlang_ctxt_pronoun");
  r_mark_precious(ctxt_pronoun_class);

  empty_names_chr = r_new_vector(r_type_character, 2);
  r_mark_precious(empty_names_chr);
  r_chr_poke(empty_names_chr, 0, r_string(""));
  r_chr_poke(empty_names_chr, 1, r_missing_str);

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
    r_base_env
  );
  r_mark_precious(restore_mask_fn);

  FREE(1);
}
