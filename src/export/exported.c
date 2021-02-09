#include <rlang.h>
#include "../internal/utils.h"
#include "../internal/vec.h"

// From rlang/vec.c
void r_vec_poke_n(sexp* x, r_ssize offset,
                  sexp* y, r_ssize from, r_ssize n);
void r_vec_poke_range(sexp* x, r_ssize offset,
                      sexp* y, r_ssize from, r_ssize to);


// attrs.c

sexp* rlang_poke_attrib(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}


// cnd.c

sexp* rlang_cnd_signal(sexp* cnd) {
  r_cnd_signal(cnd);
  return r_null;
}

sexp* rlang_cnd_type(sexp* cnd) {
  enum r_condition_type type = r_cnd_type(cnd);
  switch (type) {
  case r_cnd_type_condition: return r_chr("condition");
  case r_cnd_type_message: return r_chr("message");
  case r_cnd_type_warning: return r_chr("warning");
  case r_cnd_type_error: return r_chr("error");
  case r_cnd_type_interrupt: return r_chr("interrupt");
  default: r_abort("Internal error: Unhandled `r_condition_type`");
  }
}

sexp* rlang_interrupt() {
  r_interrupt();
  return r_null;
}


// df.c

sexp* rlang_alloc_data_frame(sexp* n_rows, sexp* names, sexp* types) {
  if (!r_is_int(n_rows)) {
    r_abort("`n_rows` must be an integer value.");
  }
  if (r_typeof(names) != r_type_character) {
    r_abort("`names` must be a character vector.");
  }
  if (r_typeof(types) != r_type_integer) {
    r_abort("`types` must be an integer vector.");
  }

  r_ssize n_rows_val = r_int_get(n_rows, 0);
  sexp* df = KEEP(r_alloc_df_list(n_rows_val,
                                  names,
                                  (enum r_type*) r_int_deref(types),
                                  r_length(names)));
  r_init_data_frame(df, n_rows_val);

  FREE(1);
  return df;
}


// dict.c

static
sexp* wrap_dict(struct r_dict* p_dict) {
  return p_dict->shelter;
}

sexp* rlang_new_dict(sexp* size, sexp* prevent_resize) {
  if (!r_is_int(size)) {
    r_abort("`size` must be an integer.");
  }
  if (!r_is_bool(prevent_resize)) {
    r_abort("`prevent_resize` must be a logical value.");
  }

  struct r_dict* dict = r_new_dict(r_int_get(size, 0));
  dict->prevent_resize = r_lgl_get(prevent_resize, 0);

  return dict->shelter;
}

static
struct r_dict* dict_deref(sexp* dict) {
  if (r_typeof(dict) != r_type_list) {
    goto dict_input_error;
  }

  sexp* dict_raw = r_list_get(dict, 0);
  if (r_typeof(dict_raw) != r_type_raw) {
    goto dict_input_error;
  }

  return r_raw_deref(dict_raw);

  dict_input_error:
    r_abort("`dict` must be a dictionary handle.");
}

sexp* rlang_dict_put(sexp* dict, sexp* key, sexp* value) {
  struct r_dict* p_dict = dict_deref(dict);
  return r_lgl(r_dict_put(p_dict, key, value));
}

sexp* rlang_dict_del(sexp* dict, sexp* key) {
  struct r_dict* p_dict = dict_deref(dict);
  return r_lgl(r_dict_del(p_dict, key));
}

sexp* rlang_dict_has(sexp* dict, sexp* key) {
  struct r_dict* p_dict = dict_deref(dict);
  return r_lgl(r_dict_has(p_dict, key));
}

sexp* rlang_dict_get(sexp* dict, sexp* key) {
  struct r_dict* p_dict = dict_deref(dict);
  return r_dict_get(p_dict, key);
}

sexp* rlang_dict_resize(sexp* dict, sexp* size) {
  if (!r_is_int(size)) {
    r_abort("`size` must be an integer.");
  }
  struct r_dict* p_dict = dict_deref(dict);

  r_dict_resize(p_dict, r_int_get(size, 0));
  return r_null;
}


// dyn-array.c

// [[ register() ]]
sexp* rlang_new_dyn_vector(sexp* type,
                           sexp* capacity) {
  struct r_dyn_array* arr = r_new_dyn_vector(r_chr_as_r_type(type),
                                             r_as_ssize(capacity));
  return arr->shelter;
}

// [[ register() ]]
sexp* rlang_new_dyn_array(sexp* elt_byte_size,
                          sexp* capacity) {
  struct r_dyn_array* arr = r_new_dyn_array(r_as_ssize(elt_byte_size),
                                            r_as_ssize(capacity));
  return arr->shelter;
}

static
struct r_dyn_array* rlang_arr_deref(sexp* arr) {
  if (r_typeof(arr) != r_type_list || r_length(arr) != 2) {
    goto err;
  }

  sexp* raw = r_list_get(arr, 0);
  if (r_typeof(raw) != r_type_raw) {
    goto err;
  }

  return r_raw_deref(raw);

 err:
  r_stop_internal("rlang_arr_deref", "Expected a dynamic array handle.");
};

// [[ register() ]]
sexp* rlang_arr_unwrap(sexp* arr) {
  return r_arr_unwrap(rlang_arr_deref(arr));
}

// [[ register() ]]
sexp* rlang_arr_info(sexp* arr_sexp) {
  struct r_dyn_array* arr = rlang_arr_deref(arr_sexp);

  const char* names_c_strs[] = {
    "count",
    "capacity",
    "growth_factor",
    "type",
    "elt_byte_size"
  };
  int info_n = R_ARR_SIZEOF(names_c_strs);

  sexp* info = KEEP(r_new_list(info_n));

  sexp* nms = r_chr_n(names_c_strs, info_n);
  r_attrib_poke_names(info, nms);

  r_list_poke(info, 0, r_dbl(arr->count));
  r_list_poke(info, 1, r_dbl(arr->capacity));
  r_list_poke(info, 2, r_int(arr->growth_factor));
  r_list_poke(info, 3, r_type_as_character(arr->type));
  r_list_poke(info, 4, r_int(arr->elt_byte_size));

  FREE(1);
  return info;
}

// [[ register() ]]
sexp* rlang_arr_push_back(sexp* arr_sexp, sexp* x) {
  struct r_dyn_array* p_arr = rlang_arr_deref(arr_sexp);

  if (!p_arr->barrier_set && r_vec_elt_sizeof(x) != p_arr->elt_byte_size) {
    r_stop_internal("rlang_arr_push_back",
                    "Incompatible byte sizes %d/%d.",
                    r_vec_elt_sizeof(x),
                    p_arr->elt_byte_size);
  }

  switch (p_arr->type) {
  case r_type_character:
  case r_type_list:
    r_arr_push_back(p_arr, x);
    return r_null;
  default:
    r_arr_push_back(p_arr, r_vec_deref(x));
    return r_null;
  }
}
// [[ register() ]]
sexp* rlang_arr_push_back_bool(sexp* arr_sexp, sexp* x_sexp) {
  struct r_dyn_array* arr = rlang_arr_deref(arr_sexp);
  bool x = r_as_bool(x_sexp);
  r_arr_push_back(arr, &x);
  return r_null;
}
// [[ register() ]]
sexp* rlang_arr_pop_back(sexp* arr_sexp) {
  struct r_dyn_array* arr = rlang_arr_deref(arr_sexp);
  r_arr_pop_back(arr);
  return r_null;
}
// [[ register() ]]
sexp* rlang_arr_resize(sexp* arr_sexp, sexp* capacity_sexp) {
  struct r_dyn_array* arr = rlang_arr_deref(arr_sexp);
  r_arr_resize(arr, r_as_ssize(capacity_sexp));
  return r_null;
}


// env.c

sexp* rlang_env_poke_parent(sexp* env, sexp* new_parent) {
  if (R_IsNamespaceEnv(env)) {
    r_abort("Can't change the parent of a namespace environment");
  }
  if (R_IsPackageEnv(env)) {
    r_abort("Can't change the parent of a package environment");
  }
  if (R_EnvironmentIsLocked(env)) {
    r_abort("Can't change the parent of a locked environment");
  }
  if (env == r_global_env) {
    r_abort("Can't change the parent of the global environment");
  }
  if (env == r_base_env) {
    r_abort("Can't change the parent of the base environment");
  }
  if (env == r_empty_env) {
    r_abort("Can't change the parent of the empty environment");
  }

  SET_ENCLOS(env, new_parent);
  return env;
}

sexp* rlang_env_frame(sexp* env) {
  return FRAME(env);
}
sexp* rlang_env_hash_table(sexp* env) {
  return HASHTAB(env);
}

sexp* rlang_env_inherits(sexp* env, sexp* ancestor) {
  return r_lgl(r_env_inherits(env, ancestor, r_empty_env));
}

sexp* rlang_env_bind_list(sexp* env, sexp* names, sexp* data) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("Internal error: `env` must be an environment.");
  }
  if (r_typeof(names) != r_type_character) {
    r_abort("Internal error: `names` must be a character vector.");
  }
  if (r_typeof(data) != r_type_list) {
    r_abort("Internal error: `data` must be a list.");
  }

  r_ssize n = r_length(data);
  if (n != r_length(names)) {
    r_abort("Internal error: `data` and `names` must have the same length.");
  }

  sexp* const * p_names = r_chr_deref_const(names);

  for (r_ssize i = 0; i < n; ++i) {
    Rf_defineVar(r_str_as_symbol(p_names[i]), r_list_get(data, i), env);
  }

  return r_null;
}

sexp* rlang_env_browse(sexp* env, sexp* value) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  if (!r_is_bool(value)) {
    r_abort("`value` must be a single logical value.");
  }

  sexp* old = r_lgl(RDEBUG(env));
  SET_RDEBUG(env, r_lgl_get(value, 0));
  return old;
}

sexp* rlang_env_is_browsed(sexp* env) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("`env` must be an environment.");
  }
  return r_lgl(RDEBUG(env));
}

sexp* rlang_ns_registry_env() {
  return R_NamespaceRegistry;
}


// eval.c

sexp* rlang_ext2_eval(sexp* call, sexp* op, sexp* args, sexp* env) {
  args = r_node_cdr(args);
  return Rf_eval(r_node_car(args), r_node_cadr(args));
}

sexp* rlang_eval_top(sexp* expr, sexp* env) {
  int jumped = 0;
  sexp* out = R_tryEval(expr, env, &jumped);

  if (jumped) {
    r_abort("Top level jump");
  } else {
    return out;
  }
}

// fn.c

sexp* rlang_is_function(sexp* x) {
  return r_shared_lgl(r_is_function(x));
}

sexp* rlang_is_closure(sexp* x) {
  return r_shared_lgl(r_typeof(x) == r_type_closure);
}

sexp* rlang_is_primitive(sexp* x) {
  return r_shared_lgl(r_is_primitive(x));
}
sexp* rlang_is_primitive_lazy(sexp* x) {
  return r_shared_lgl(r_typeof(x) == r_type_special);
}
sexp* rlang_is_primitive_eager(sexp* x) {
  return r_shared_lgl(r_typeof(x) == r_type_builtin);
}


// formula.c

static
int as_optional_bool(sexp* lgl) {
  if (lgl == r_null) {
    return -1;
  } else {
    return r_lgl_get(lgl, 0);
  }
}

sexp* rlang_is_formula(sexp* x, sexp* scoped, sexp* lhs) {
  int scoped_int = as_optional_bool(scoped);
  int lhs_int = as_optional_bool(lhs);

  bool out = r_is_formula(x, scoped_int, lhs_int);
  return r_lgl(out);
}

bool is_formulaish(sexp* x, int scoped, int lhs);
sexp* rlang_is_formulaish(sexp* x, sexp* scoped, sexp* lhs) {
  int scoped_int = as_optional_bool(scoped);
  int lhs_int = as_optional_bool(lhs);

  bool out = is_formulaish(x, scoped_int, lhs_int);
  return r_lgl(out);
}


// parse.c

#include "../internal/parse.h"

sexp* rlang_call_has_precedence(sexp* x, sexp* y, sexp* side) {
  int c_side = r_int_get(side, 0);

  bool has_predence;
  switch (c_side) {
  case -1:
    has_predence = r_lhs_call_has_precedence(x, y);
    break;
  case 0:
    has_predence = r_call_has_precedence(x, y);
    break;
  case 1:
    has_predence = r_rhs_call_has_precedence(x, y);
    break;
  default:
    r_stop_internal("rlang_call_has_precedence", "Unexpected `side` value.");
  }
  return r_lgl(has_predence);
}

sexp* rlang_which_operator(sexp* call) {
  const char* op = r_op_as_c_string(r_which_operator(call));
  return r_chr(op);
}


// node.c

sexp* rlang_node_car(sexp* x) {
  return CAR(x);
}
sexp* rlang_node_cdr(sexp* x) {
  return CDR(x);
}
sexp* rlang_node_caar(sexp* x) {
  return CAAR(x);
}
sexp* rlang_node_cadr(sexp* x) {
  return CADR(x);
}
sexp* rlang_node_cdar(sexp* x) {
  return CDAR(x);
}
sexp* rlang_node_cddr(sexp* x) {
  return CDDR(x);
}
sexp* rlang_node_tail(sexp* x) {
  while (CDR(x) != r_null)
    x = CDR(x);
  return x;
}

sexp* rlang_node_poke_car(sexp* x, sexp* newcar) {
  SETCAR(x, newcar);
  return x;
}
sexp* rlang_node_poke_cdr(sexp* x, sexp* newcdr) {
  SETCDR(x, newcdr);
  return x;
}
sexp* rlang_node_poke_caar(sexp* x, sexp* newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
sexp* rlang_node_poke_cadr(sexp* x, sexp* newcar) {
  SETCADR(x, newcar);
  return x;
}
sexp* rlang_node_poke_cdar(sexp* x, sexp* newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
sexp* rlang_node_poke_cddr(sexp* x, sexp* newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

sexp* rlang_node_tag(sexp* x) {
  return TAG(x);
}
sexp* rlang_node_poke_tag(sexp* x, sexp* tag) {
  SET_TAG(x, tag);
  return x;
}

sexp* rlang_on_exit(sexp* expr, sexp* frame) {
  r_on_exit(expr, frame);
  return r_null;
}


// lang.h

sexp* rlang_new_call_node(sexp* car, sexp* cdr) {
  return Rf_lcons(car, cdr);
}


// quo.h

#include "../internal/quo.h"

sexp* rlang_quo_is_missing(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_missing(quo));
}
sexp* rlang_quo_is_symbol(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_symbol(quo));
}
sexp* rlang_quo_is_call(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_call(quo));
}
sexp* rlang_quo_is_symbolic(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_symbolic(quo));
}
sexp* rlang_quo_is_null(sexp* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_null(quo));
}


// sexp.h

sexp* rlang_length(sexp* x) {
  return r_int(r_length(x));
}
sexp* rlang_true_length(sexp* x) {
  return r_int(XTRUELENGTH(x));
}

sexp* rlang_is_reference(sexp* x, sexp* y) {
  return r_lgl(x == y);
}

sexp* rlang_missing_arg() {
  return R_MissingArg;
}

sexp* rlang_duplicate(sexp* x, sexp* shallow) {
  if (r_lgl_get(shallow, 0)) {
    return r_clone(x);
  } else {
    return r_copy(x);
  }
}

sexp* rlang_sexp_address(sexp* x) {
  return r_str_as_character(r_sexp_address(x));
}

sexp* rlang_poke_type(sexp* x, sexp* type) {
  SET_TYPEOF(x, Rf_str2type(r_chr_get_c_string(type, 0)));
  return x;
}

sexp* rlang_mark_object(sexp* x) {
  SET_OBJECT(x, 1);
  return x;
}
sexp* rlang_unmark_object(sexp* x) {
  SET_OBJECT(x, 0);
  return x;
}

sexp* rlang_get_promise(sexp* x, sexp* env) {
  switch (r_typeof(x)) {
  case r_type_promise:
    return x;
  case r_type_character:
    if (r_length(x) == 1) {
      x = r_sym(r_chr_get_c_string(x, 0));
    } else {
      goto error;
    }
    // fallthrough
  case r_type_symbol: {
      sexp* prom = r_env_find_anywhere(env, x);
      if (r_typeof(prom) == r_type_promise) {
        return prom;
      }
      // fallthrough
    }
  error:
  default:
    r_abort("`x` must be or refer to a local promise");
  }
}

sexp* rlang_promise_expr(sexp* x, sexp* env) {
  sexp* prom = rlang_get_promise(x, env);
  return PREXPR(prom);
}
sexp* rlang_promise_env(sexp* x, sexp* env) {
  sexp* prom = rlang_get_promise(x, env);
  return PRENV(prom);
}
sexp* rlang_promise_value(sexp* x, sexp* env) {
  sexp* prom = rlang_get_promise(x, env);
  sexp* value = PRVALUE(prom);
  if (value == r_syms_unbound) {
    return r_sym("R_UnboundValue");
  } else {
    return value;
  }
}

sexp* rlang_attrib(sexp* x) {
  return ATTRIB(x);
}

// Picks up symbols from parent environment to avoid bumping namedness
// during promise resolution
sexp* rlang_named(sexp* x, sexp* env) {
  int n_kept = 0;

  x = PROTECT(Rf_findVarInFrame3(env, x, FALSE));
  ++n_kept;

  if (TYPEOF(x) == PROMSXP) {
    x = PROTECT(Rf_eval(x, env));
    ++n_kept;
  }

  UNPROTECT(n_kept);
  return Rf_ScalarInteger(NAMED(x));
}

sexp* rlang_find_var(sexp* env, sexp* sym) {
  return Rf_findVar(sym, env);
}

sexp* rlang_chr_get(sexp* x, sexp* i) {
  if (r_typeof(i) != r_type_integer || r_length(i) != 1) {
    r_abort("`i` must be an integer value.");
  }

  int c_i = r_int_get(i, 0);
  if (c_i < 0 || c_i >= r_length(x)) {
    r_abort("`i` is out of bound. Note that `r_chr_get()` takes zero-based locations.");
  }

  return r_chr_get(x, c_i);
}

// Returns a copy
sexp* rlang_precious_dict() {
  // From rlang/sexp.c
  struct r_dict* rlang__precious_dict();

  struct r_dict* p_dict = rlang__precious_dict();
  return wrap_dict(p_dict);
}
sexp* rlang_preserve(sexp* x) {
  r_preserve(x);
  return r_null;
}
sexp* rlang_unpreserve(sexp* x) {
  r_unpreserve(x);
  return r_null;
}


// vec.h

sexp* rlang_vec_alloc(sexp* type, sexp* n) {
  return Rf_allocVector(Rf_str2type(r_chr_get_c_string(type, 0)), r_int_get(n, 0));
}
sexp* rlang_vec_coerce(sexp* x, sexp* type) {
  return Rf_coerceVector(x, Rf_str2type(r_chr_get_c_string(type, 0)));
}

sexp* rlang_vec_poke_n(sexp* x, sexp* offset,
                       sexp* y, sexp* from, sexp* n) {
  r_ssize offset_size = r_as_ssize(offset) - 1;
  r_ssize from_size = r_as_ssize(from) - 1;
  r_ssize n_size = r_as_ssize(n);

  r_vec_poke_n(x, offset_size, y, from_size, n_size);
  return x;
}

sexp* rlang_vec_poke_range(sexp* x, sexp* offset,
                           sexp* y, sexp* from, sexp* to) {
  r_ssize offset_size = r_as_ssize(offset) - 1;
  r_ssize from_size = r_as_ssize(from) - 1;
  r_ssize to_size = r_as_ssize(to) - 1;

  r_vec_poke_range(x, offset_size, y, from_size, to_size);
  return x;
}

static r_ssize validate_n(sexp* n) {
  if (n == r_null) {
    return -1;
  }

  switch (r_typeof(n)) {
  case r_type_integer:
  case r_type_double:
    if (r_length(n) == 1) {
      break;
    }
    // fallthrough
  default:
    r_abort("`n` must be NULL or a scalar integer");
  }

  return r_as_ssize(n);
}

static int validate_finite(sexp* finite) {
  switch (r_typeof(finite)) {
  case r_type_null:
    return -1;
  case r_type_integer:
  case r_type_double:
    finite = r_vec_coerce(finite, r_type_logical);
  case r_type_logical: {
    int value = r_lgl_get(finite, 0);
    if (value != r_lgls_na) {
      return r_lgl_get(finite, 0);
    } // else fallthrough
  }
  default:
    r_abort("`finite` must be NULL or a scalar logical");
  }
}

sexp* rlang_is_finite(sexp* x) {
  return r_shared_lgl(r_is_finite(x));
}

sexp* rlang_is_list(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  if (r_typeof(x) != r_type_list) {
    return r_false;
  }
  if (n < 0) {
    return r_true;
  }
  return r_shared_lgl(r_length(x) == n);
}

sexp* rlang_is_atomic(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_atomic(x, n));
}
sexp* rlang_is_vector(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_vector(x, n));
}

sexp* rlang_is_logical(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_logical(x, n));
}
sexp* rlang_is_integer(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_integer(x, n, -1));
}
sexp* rlang_is_double(sexp* x, sexp* n_, sexp* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_double(x, n, finite));
}
sexp* rlang_is_integerish(sexp* x, sexp* n_, sexp* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_integerish(x, n, finite));
}

sexp* rlang_is_character(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_character(x, n));
}
sexp* rlang_is_raw(sexp* x, sexp* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_raw(x, n));
}

sexp* rlang_is_string(sexp* x, sexp* string) {
  if (r_typeof(x) != r_type_character || r_length(x) != 1) {
    return r_false;
  }

  sexp* value = r_chr_get(x, 0);

  if (value == r_strs_na) {
    return r_false;
  }

  if (string == r_null) {
    return r_true;
  }

  if (!rlang_is_string(string, r_null)) {
    r_abort("`string` must be `NULL` or a string");
  }

  bool out = false;
  r_ssize n = r_length(string);
  sexp* const * p_string = r_chr_deref_const(string);

  for (r_ssize i = 0; i < n; ++i) {
    if (p_string[i] == value) {
      out = true;
      break;
    }
  }

  return r_shared_lgl(out);
}

sexp* rlang_vec_resize(sexp* x, sexp* n) {
  r_ssize n_ssize = r_as_ssize(n);

  switch (r_typeof(x)) {
  case r_type_logical: return r_lgl_resize(x, n_ssize);
  case r_type_integer: return r_int_resize(x, n_ssize);
  case r_type_double: return r_dbl_resize(x, n_ssize);
  case r_type_complex: return r_cpl_resize(x, n_ssize);
  case r_type_raw: return r_raw_resize(x, n_ssize);
  case r_type_character: return r_chr_resize(x, n_ssize);
  case r_type_list: return r_list_resize(x, n_ssize);
  default: r_stop_unimplemented_type("rlang_vec_resize", r_typeof(x));
  }
}

sexp* rlang_list_poke(sexp* x, sexp* i, sexp* value) {
  r_list_poke(x, r_as_ssize(i), value);
  return r_null;
}
