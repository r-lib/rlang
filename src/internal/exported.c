#include <rlang.h>
#include "internal.h"
#include "utils.h"
#include "vec.h"

// From rlang/vec.c
void r_vec_poke_n(r_obj* x, r_ssize offset,
                  r_obj* y, r_ssize from, r_ssize n);
void r_vec_poke_range(r_obj* x, r_ssize offset,
                      r_obj* y, r_ssize from, r_ssize to);


r_obj* ffi_compiled_by_gcc(void) {
#if defined(__GNUC__) && !defined(__clang__)
  return r_true;
  #else
  return r_false;
  #endif
}


// cnd.c

r_obj* ffi_cnd_signal(r_obj* cnd) {
  r_cnd_signal(cnd);
  return r_null;
}

r_obj* ffi_cnd_type(r_obj* cnd) {
  enum r_cnd_type type = r_cnd_type(cnd);
  switch (type) {
  case R_CND_TYPE_condition: return r_chr("condition");
  case R_CND_TYPE_message: return r_chr("message");
  case R_CND_TYPE_warning: return r_chr("warning");
  case R_CND_TYPE_error: return r_chr("error");
  case R_CND_TYPE_interrupt: return r_chr("interrupt");
  default: r_abort("Internal error: Unhandled `r_condition_type`");
  }
}

r_obj* ffi_interrupt(void) {
  r_interrupt();
  return r_null;
}


// df.c

r_obj* ffi_alloc_data_frame(r_obj* n_rows, r_obj* names, r_obj* types) {
  if (!r_is_int(n_rows)) {
    r_abort("`n_rows` must be an integer value.");
  }
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("`names` must be a character vector.");
  }
  if (r_typeof(types) != R_TYPE_integer) {
    r_abort("`types` must be an integer vector.");
  }

  r_ssize n_rows_val = r_int_get(n_rows, 0);
  r_obj* df = KEEP(r_alloc_df_list(n_rows_val,
                                   names,
                                   (enum r_type*) r_int_begin(types),
                                   r_length(names)));
  r_init_data_frame(df, n_rows_val);

  FREE(1);
  return df;
}


// dict.c

static
r_obj* wrap_dict(struct r_dict* p_dict) {
  return p_dict->shelter;
}

r_obj* ffi_new_dict(r_obj* size, r_obj* prevent_resize) {
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

r_obj* ffi_dict_poke(r_obj* dict, r_obj* key, r_obj* value) {
  struct r_dict* p_dict = r_shelter_deref(dict);
  r_obj* out = r_dict_poke(p_dict, key, value);
  return out ? out : rlang_syms.c_null;
}

r_obj* ffi_dict_put(r_obj* dict, r_obj* key, r_obj* value) {
  struct r_dict* p_dict = r_shelter_deref(dict);
  return r_lgl(r_dict_put(p_dict, key, value));
}

r_obj* ffi_dict_del(r_obj* dict, r_obj* key) {
  struct r_dict* p_dict = r_shelter_deref(dict);
  return r_lgl(r_dict_del(p_dict, key));
}

r_obj* ffi_dict_has(r_obj* dict, r_obj* key) {
  struct r_dict* p_dict = r_shelter_deref(dict);
  return r_lgl(r_dict_has(p_dict, key));
}

r_obj* ffi_dict_get(r_obj* dict, r_obj* key) {
  struct r_dict* p_dict = r_shelter_deref(dict);
  return r_dict_get(p_dict, key);
}

r_obj* ffi_dict_resize(r_obj* dict, r_obj* size) {
  if (!r_is_int(size)) {
    r_abort("`size` must be an integer.");
  }
  struct r_dict* p_dict = r_shelter_deref(dict);

  r_dict_resize(p_dict, r_int_get(size, 0));
  return r_null;
}

r_obj* ffi_dict_as_df_list(r_obj* dict) {
  return r_dict_as_df_list(r_shelter_deref(dict));
}
r_obj* ffi_dict_as_list(r_obj* dict) {
  return r_dict_as_list(r_shelter_deref(dict));
}

r_obj* ffi_new_dict_iterator(r_obj* dict) {
  struct r_dict* p_dict = r_shelter_deref(dict);
  return r_new_dict_iterator(p_dict)->shelter;
}
r_obj* ffi_dict_it_info(r_obj* dict_it) {
  struct r_dict_iterator* p_it = r_shelter_deref(dict_it);

  const char* v_nms[] = {
    "key",
    "value",
    "i",
    "n"
  };
  int n = R_ARR_SIZEOF(v_nms);

  r_obj* info = KEEP(r_alloc_list(n));
  r_attrib_poke_names(info, r_chr_n(v_nms, n));
  r_list_poke(info, 0, p_it->key);
  r_list_poke(info, 1, p_it->value);
  r_list_poke(info, 2, r_len(p_it->i));
  r_list_poke(info, 3, r_len(p_it->n));

  FREE(1);
  return info;
}
r_obj* ffi_dict_it_next(r_obj* dict_it) {
  struct r_dict_iterator* p_dict_it = r_shelter_deref(dict_it);
  return r_lgl(r_dict_next(p_dict_it));
}


// dyn-array.c

// [[ register() ]]
r_obj* ffi_new_dyn_vector(r_obj* type,
                          r_obj* capacity) {
  struct r_dyn_array* arr = r_new_dyn_vector(r_chr_as_r_type(type),
                                             r_arg_as_ssize(capacity, "capacity"));
  return arr->shelter;
}

// [[ register() ]]
r_obj* ffi_new_dyn_array(r_obj* elt_byte_size,
                         r_obj* capacity) {
  struct r_dyn_array* arr = r_new_dyn_array(r_arg_as_ssize(elt_byte_size, "elt_byte_size"),
                                            r_arg_as_ssize(capacity, "capacity"));
  return arr->shelter;
}

// [[ register() ]]
r_obj* ffi_dyn_unwrap(r_obj* arr) {
  return r_dyn_unwrap(r_shelter_deref(arr));
}

// [[ register() ]]
r_obj* ffi_dyn_info(r_obj* arr_sexp) {
  struct r_dyn_array* arr = r_shelter_deref(arr_sexp);

  const char* names_c_strs[] = {
    "count",
    "capacity",
    "growth_factor",
    "type",
    "elt_byte_size"
  };
  int info_n = R_ARR_SIZEOF(names_c_strs);

  r_obj* info = KEEP(r_alloc_list(info_n));

  r_obj* nms = r_chr_n(names_c_strs, info_n);
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
r_obj* ffi_dyn_push_back(r_obj* arr_sexp, r_obj* x) {
  struct r_dyn_array* p_arr = r_shelter_deref(arr_sexp);

  if (!p_arr->barrier_set && r_vec_elt_sizeof(x) != p_arr->elt_byte_size) {
    r_stop_internal("Incompatible byte sizes %d/%d.",
                    r_vec_elt_sizeof(x),
                    p_arr->elt_byte_size);
  }

  switch (p_arr->type) {
  case R_TYPE_character:
  case R_TYPE_list:
    r_dyn_push_back(p_arr, &x);
    return r_null;
  default:
    r_dyn_push_back(p_arr, r_vec_cbegin(x));
    return r_null;
  }
}
// [[ register() ]]
r_obj* ffi_dyn_push_back_bool(r_obj* arr_sexp, r_obj* x_sexp) {
  struct r_dyn_array* arr = r_shelter_deref(arr_sexp);
  bool x = r_as_bool(x_sexp);
  r_dyn_push_back(arr, &x);
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_pop_back(r_obj* arr_sexp) {
  struct r_dyn_array* arr = r_shelter_deref(arr_sexp);
  void* const * out = r_dyn_pop_back(arr);

  if (arr->type == R_TYPE_list) {
    return *((r_obj* const *) out);
  } else {
    return r_null;
  }
}
// [[ register() ]]
r_obj* ffi_dyn_resize(r_obj* arr_sexp, r_obj* capacity_sexp) {
  struct r_dyn_array* arr = r_shelter_deref(arr_sexp);
  r_dyn_resize(arr, r_arg_as_ssize(capacity_sexp, "capacity"));
  return r_null;
}

// [[ register() ]]
r_obj* ffi_dyn_lgl_get(r_obj* x, r_obj* i) {
  return r_lgl(r_dyn_lgl_get(r_shelter_deref(x), r_arg_as_ssize(i, "i")));
}
// [[ register() ]]
r_obj* ffi_dyn_int_get(r_obj* x, r_obj* i) {
  return r_int(r_dyn_int_get(r_shelter_deref(x), r_arg_as_ssize(i, "i")));
}
// [[ register() ]]
r_obj* ffi_dyn_dbl_get(r_obj* x, r_obj* i) {
  return r_dbl(r_dyn_dbl_get(r_shelter_deref(x), r_arg_as_ssize(i, "i")));
}
// [[ register() ]]
r_obj* ffi_dyn_cpl_get(r_obj* x, r_obj* i) {
  return r_cpl(r_dyn_cpl_get(r_shelter_deref(x), r_arg_as_ssize(i, "i")));
}
// [[ register() ]]
r_obj* ffi_dyn_raw_get(r_obj* x, r_obj* i) {
  return r_raw(r_dyn_raw_get(r_shelter_deref(x), r_arg_as_ssize(i, "i")));
}
// [[ register() ]]
r_obj* ffi_dyn_chr_get(r_obj* x, r_obj* i) {
  return r_dyn_chr_get(r_shelter_deref(x), r_arg_as_ssize(i, "i"));
}
// [[ register() ]]
r_obj* ffi_dyn_list_get(r_obj* x, r_obj* i) {
  return r_dyn_list_get(r_shelter_deref(x), r_arg_as_ssize(i, "i"));
}

// [[ register() ]]
r_obj* ffi_dyn_lgl_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_lgl_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), r_as_bool(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_int_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_int_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), r_as_int(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_dbl_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_dbl_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), r_as_double(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_cpl_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_cpl_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), r_as_complex(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_raw_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_raw_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), r_as_char(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_chr_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_chr_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), value);
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_list_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_dyn_list_poke(r_shelter_deref(x), r_arg_as_ssize(i, "i"), value);
  return r_null;
}

// [[ register() ]]
r_obj* ffi_dyn_lgl_push_back(r_obj* x, r_obj* value) {
  r_dyn_lgl_push_back(r_shelter_deref(x), r_as_bool(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_int_push_back(r_obj* x, r_obj* value) {
  r_dyn_int_push_back(r_shelter_deref(x), r_as_int(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_dbl_push_back(r_obj* x, r_obj* value) {
  r_dyn_dbl_push_back(r_shelter_deref(x), r_as_double(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_cpl_push_back(r_obj* x, r_obj* value) {
  r_dyn_cpl_push_back(r_shelter_deref(x), r_as_complex(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_raw_push_back(r_obj* x, r_obj* value) {
  r_dyn_raw_push_back(r_shelter_deref(x), r_as_char(value));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_chr_push_back(r_obj* x, r_obj* value) {
  r_dyn_chr_push_back(r_shelter_deref(x), value);
  return r_null;
}
// [[ register() ]]
r_obj* ffi_dyn_list_push_back(r_obj* x, r_obj* value) {
  r_dyn_list_push_back(r_shelter_deref(x), value);
  return r_null;
}

// [[ register() ]]
r_obj* ffi_has_size_one_bool(void) {
  return r_lgl(sizeof(bool) == 1);
}


// dyn-list-of.c

// [[ register() ]]
r_obj* ffi_new_dyn_list_of(r_obj* type, r_obj* capacity, r_obj* width) {
  struct r_dyn_list_of* lof = r_new_dyn_list_of(r_chr_as_r_type(type),
                                                r_arg_as_ssize(capacity, "capacity"),
                                                r_arg_as_ssize(width, "width"));
  return lof->shelter;
}

enum info_lof {
  INFO_LOF_count,
  INFO_LOF_growth_factor,
  INFO_LOF_arrays,
  INFO_LOF_width,
  INFO_LOF_reserve,
  INFO_LOF_capacity,
  INFO_LOF_moved_array,
  INFO_LOF_type,
  INFO_LOF_elt_byte_size,
  INFO_LOF_SIZE
};
static
const char* info_lof_c_strs[INFO_LOF_SIZE] = {
  "count",
  "growth_factor",
  "arrays",
  "width",
  "reserve",
  "capacity",
  "moved_array",
  "type",
  "elt_byte_size",
};

// [[ register() ]]
r_obj* ffi_lof_info(r_obj* lof) {
  struct r_dyn_list_of* p_lof = r_shelter_deref(lof);

  r_obj* info = KEEP(r_alloc_list(INFO_LOF_SIZE));

  r_obj* nms = r_chr_n(info_lof_c_strs, INFO_LOF_SIZE);
  r_attrib_poke_names(info, nms);

  r_list_poke(info, INFO_LOF_count, r_dbl(p_lof->count));
  r_list_poke(info, INFO_LOF_growth_factor, r_int(p_lof->growth_factor));
  r_list_poke(info, INFO_LOF_arrays, r_lof_unwrap(p_lof));
  r_list_poke(info, INFO_LOF_width, r_len(p_lof->width));
  r_list_poke(info, INFO_LOF_reserve, p_lof->reserve);
  r_list_poke(info, INFO_LOF_capacity, r_len(p_lof->capacity));
  r_list_poke(info, INFO_LOF_moved_array, p_lof->p_moved_arr->shelter);
  r_list_poke(info, INFO_LOF_type, r_type_as_character(p_lof->type));
  r_list_poke(info, INFO_LOF_elt_byte_size, r_int(p_lof->elt_byte_size));

  FREE(1);
  return info;
}

// [[ register() ]]
r_obj* ffi_lof_unwrap(r_obj* lof) {
  return r_lof_unwrap(r_shelter_deref(lof));
}

// [[ register() ]]
r_obj* ffi_lof_push_back(r_obj* lof) {
  r_lof_push_back(r_shelter_deref(lof));
  return r_null;
}
// [[ register() ]]
r_obj* ffi_lof_arr_push_back(r_obj* lof, r_obj* i, r_obj* value) {
  struct r_dyn_list_of* p_lof = r_shelter_deref(lof);
  if (r_typeof(value) != p_lof->type) {
    r_abort("Can't push value of type %s in dyn-list-of %s",
            r_type_as_c_string(r_typeof(value)),
            r_type_as_c_string(p_lof->type));
  }
  r_lof_arr_push_back(p_lof,
                      r_arg_as_ssize(i, "i"),
                      r_vec_begin(value));
  return r_null;
}


// env.c

r_obj* ffi_env_poke_parent(r_obj* env, r_obj* new_parent) {
  if (R_IsNamespaceEnv(env)) {
    r_abort("Can't change the parent of a namespace environment");
  }
  if (R_IsPackageEnv(env)) {
    r_abort("Can't change the parent of a package environment");
  }
  if (R_EnvironmentIsLocked(env)) {
    r_abort("Can't change the parent of a locked environment");
  }
  if (env == r_envs.global) {
    r_abort("Can't change the parent of the global environment");
  }
  if (env == r_envs.base) {
    r_abort("Can't change the parent of the base environment");
  }
  if (env == r_envs.empty) {
    r_abort("Can't change the parent of the empty environment");
  }

  SET_ENCLOS(env, new_parent);
  return env;
}

r_obj* ffi_env_inherits(r_obj* env, r_obj* ancestor) {
  return r_lgl(r_env_inherits(env, ancestor, r_envs.empty));
}

r_obj* ffi_env_bind_list(r_obj* env, r_obj* names, r_obj* data) {
  if (r_typeof(env) != R_TYPE_environment) {
    r_abort("Internal error: `env` must be an environment.");
  }
  if (r_typeof(names) != R_TYPE_character) {
    r_abort("Internal error: `names` must be a character vector.");
  }
  if (r_typeof(data) != R_TYPE_list) {
    r_abort("Internal error: `data` must be a list.");
  }

  r_ssize n = r_length(data);
  if (n != r_length(names)) {
    r_abort("Internal error: `data` and `names` must have the same length.");
  }

  r_obj* const * p_names = r_chr_cbegin(names);

  for (r_ssize i = 0; i < n; ++i) {
    Rf_defineVar(r_str_as_symbol(p_names[i]), r_list_get(data, i), env);
  }

  return r_null;
}

r_obj* ffi_ns_registry_env(void) {
  return R_NamespaceRegistry;
}


// eval.c

r_obj* ffi_eval(r_obj* call, r_obj* op, r_obj* args, r_obj* env) {
  args = r_node_cdr(args);
  return Rf_eval(r_node_car(args), r_node_cadr(args));
}

r_obj* ffi_eval_top(r_obj* expr, r_obj* env) {
  int jumped = 0;
  r_obj* out = R_tryEval(expr, env, &jumped);

  if (jumped) {
    r_abort("Top level jump");
  } else {
    return out;
  }
}

// fn.c

r_obj* ffi_is_function(r_obj* x) {
  return r_shared_lgl(r_is_function(x));
}

r_obj* ffi_is_closure(r_obj* x) {
  return r_shared_lgl(r_typeof(x) == R_TYPE_closure);
}

r_obj* ffi_is_primitive(r_obj* x) {
  return r_shared_lgl(r_is_primitive(x));
}
r_obj* ffi_is_primitive_lazy(r_obj* x) {
  return r_shared_lgl(r_typeof(x) == R_TYPE_special);
}
r_obj* ffi_is_primitive_eager(r_obj* x) {
  return r_shared_lgl(r_typeof(x) == R_TYPE_builtin);
}


// formula.c

static
int as_optional_bool(r_obj* lgl) {
  if (lgl == r_null) {
    return -1;
  } else {
    return r_lgl_get(lgl, 0);
  }
}
// [[ register() ]]
r_obj* ffi_is_formula(r_obj* x, r_obj* scoped, r_obj* lhs) {
  int scoped_int = as_optional_bool(scoped);
  int lhs_int = as_optional_bool(lhs);
  return r_lgl(r_is_formula(x, scoped_int, lhs_int));
}


// parse.c

#include "../internal/parse.h"

r_obj* ffi_call_has_precedence(r_obj* x, r_obj* y, r_obj* side) {
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
    r_stop_internal("Unexpected `side` value.");
  }
  return r_lgl(has_predence);
}

r_obj* ffi_which_operator(r_obj* call) {
  const char* op = r_op_as_c_string(r_which_operator(call));
  return r_chr(op);
}


// node.c

r_obj* ffi_node_car(r_obj* x) {
  return CAR(x);
}
r_obj* ffi_node_cdr(r_obj* x) {
  return CDR(x);
}
r_obj* ffi_node_caar(r_obj* x) {
  return CAAR(x);
}
r_obj* ffi_node_cadr(r_obj* x) {
  return CADR(x);
}
r_obj* ffi_node_cdar(r_obj* x) {
  return CDAR(x);
}
r_obj* ffi_node_cddr(r_obj* x) {
  return CDDR(x);
}
r_obj* ffi_node_tail(r_obj* x) {
  while (CDR(x) != r_null)
    x = CDR(x);
  return x;
}

r_obj* ffi_node_poke_car(r_obj* x, r_obj* newcar) {
  SETCAR(x, newcar);
  return x;
}
r_obj* ffi_node_poke_cdr(r_obj* x, r_obj* newcdr) {
  SETCDR(x, newcdr);
  return x;
}
r_obj* ffi_node_poke_caar(r_obj* x, r_obj* newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
r_obj* ffi_node_poke_cadr(r_obj* x, r_obj* newcar) {
  SETCADR(x, newcar);
  return x;
}
r_obj* ffi_node_poke_cdar(r_obj* x, r_obj* newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
r_obj* ffi_node_poke_cddr(r_obj* x, r_obj* newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

r_obj* ffi_node_tag(r_obj* x) {
  return TAG(x);
}
r_obj* ffi_node_poke_tag(r_obj* x, r_obj* tag) {
  SET_TAG(x, tag);
  return x;
}

r_obj* rlang_on_exit(r_obj* expr, r_obj* frame) {
  r_on_exit(expr, frame);
  return r_null;
}


// lang.h

r_obj* ffi_new_call_node(r_obj* car, r_obj* cdr) {
  return Rf_lcons(car, cdr);
}


// quo.h

#include "../internal/quo.h"

r_obj* ffi_quo_is_missing(r_obj* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_missing(quo));
}
r_obj* ffi_quo_is_symbol(r_obj* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_symbol(quo));
}
r_obj* ffi_quo_is_call(r_obj* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_call(quo));
}
r_obj* ffi_quo_is_symbolic(r_obj* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_symbolic(quo));
}
r_obj* ffi_quo_is_null(r_obj* quo) {
  check_quosure(quo);
  return r_lgl(quo_is_null(quo));
}


// sexp.h

r_obj* ffi_length(r_obj* x) {
  return r_int(r_length(x));
}
r_obj* ffi_true_length(r_obj* x) {
  return r_int(XTRUELENGTH(x));
}

r_obj* ffi_is_reference(r_obj* x, r_obj* y) {
  return r_lgl(x == y);
}

r_obj* ffi_missing_arg(void) {
  return R_MissingArg;
}

r_obj* ffi_duplicate(r_obj* x, r_obj* shallow) {
  if (r_lgl_get(shallow, 0)) {
    return r_clone(x);
  } else {
    return r_copy(x);
  }
}

r_obj* ffi_obj_address(r_obj* x) {
  return r_str_as_character(r_obj_address(x));
}

r_obj* ffi_poke_type(r_obj* x, r_obj* type) {
  SET_TYPEOF(x, Rf_str2type(r_chr_get_c_string(type, 0)));
  return x;
}

r_obj* ffi_mark_object(r_obj* x) {
  SET_OBJECT(x, 1);
  return x;
}
r_obj* ffi_unmark_object(r_obj* x) {
  SET_OBJECT(x, 0);
  return x;
}

r_obj* rlang_get_promise(r_obj* x, r_obj* env) {
  switch (r_typeof(x)) {
  case R_TYPE_promise:
    return x;
  case R_TYPE_character:
    if (r_length(x) == 1) {
      x = r_sym(r_chr_get_c_string(x, 0));
    } else {
      goto error;
    }
    // fallthrough
  case R_TYPE_symbol: {
    r_obj* prom = r_env_find_anywhere(env, x);
    if (r_typeof(prom) == R_TYPE_promise) {
      return prom;
    }
    // fallthrough
  }
  error:
  default:
    r_abort("`x` must be or refer to a local promise");
  }
}

r_obj* ffi_promise_expr(r_obj* x, r_obj* env) {
  r_obj* prom = rlang_get_promise(x, env);
  return PREXPR(prom);
}
r_obj* ffi_promise_env(r_obj* x, r_obj* env) {
  r_obj* prom = rlang_get_promise(x, env);
  return PRENV(prom);
}
r_obj* ffi_promise_value(r_obj* x, r_obj* env) {
  r_obj* prom = rlang_get_promise(x, env);
  r_obj* value = PRVALUE(prom);
  if (value == r_syms.unbound) {
    return r_sym("R_UnboundValue");
  } else {
    return value;
  }
}

r_obj* ffi_find_var(r_obj* env, r_obj* sym) {
  return Rf_findVar(sym, env);
}
r_obj* ffi_find_var_in_frame(r_obj* env, r_obj* sym) {
  return Rf_findVarInFrame(env, sym);
}

r_obj* ffi_chr_get(r_obj* x, r_obj* i) {
  if (r_typeof(i) != R_TYPE_integer || r_length(i) != 1) {
    r_abort("`i` must be an integer value.");
  }

  int c_i = r_int_get(i, 0);
  if (c_i < 0 || c_i >= r_length(x)) {
    r_abort("`i` is out of bound. Note that `r_chr_get()` takes zero-based locations.");
  }

  return r_chr_get(x, c_i);
}

// Returns a copy
r_obj* ffi_precious_dict(void) {
  // From rlang/sexp.c
  struct r_dict* rlang__precious_dict(void);

  struct r_dict* p_dict = rlang__precious_dict();
  return wrap_dict(p_dict);
}
r_obj* ffi_preserve(r_obj* x) {
  r_preserve(x);
  return r_null;
}
r_obj* ffi_unpreserve(r_obj* x) {
  r_unpreserve(x);
  return r_null;
}


// vec.h

r_obj* ffi_vec_alloc(r_obj* type, r_obj* n) {
  return Rf_allocVector(Rf_str2type(r_chr_get_c_string(type, 0)), r_int_get(n, 0));
}
r_obj* ffi_vec_coerce(r_obj* x, r_obj* type) {
  return Rf_coerceVector(x, Rf_str2type(r_chr_get_c_string(type, 0)));
}

r_obj* ffi_vec_poke_n(r_obj* x, r_obj* offset,
                        r_obj* y, r_obj* from, r_obj* n) {
  r_ssize offset_size = r_arg_as_ssize(offset, "offset") - 1;
  r_ssize from_size = r_arg_as_ssize(from, "from") - 1;
  r_ssize n_size = r_arg_as_ssize(n, "n");

  r_vec_poke_n(x, offset_size, y, from_size, n_size);
  return x;
}

r_obj* ffi_vec_poke_range(r_obj* x, r_obj* offset,
                            r_obj* y, r_obj* from, r_obj* to) {
  r_ssize offset_size = r_arg_as_ssize(offset, "offset") - 1;
  r_ssize from_size = r_arg_as_ssize(from, "from") - 1;
  r_ssize to_size = r_arg_as_ssize(to, "to") - 1;

  r_vec_poke_range(x, offset_size, y, from_size, to_size);
  return x;
}

static int validate_finite(r_obj* finite) {
  switch (r_typeof(finite)) {
  case R_TYPE_null:
    return -1;
  case R_TYPE_integer:
  case R_TYPE_double:
    finite = r_vec_coerce(finite, R_TYPE_logical);
  case R_TYPE_logical: {
    int value = r_lgl_get(finite, 0);
    if (value != r_globals.na_lgl) {
      return r_lgl_get(finite, 0);
    } // else fallthrough
  }
  default:
    r_abort("`finite` must be NULL or a scalar logical");
  }
}

r_obj* ffi_is_finite(r_obj* x) {
  return r_shared_lgl(_r_is_finite(x));
}

r_obj* ffi_is_list(r_obj* x, r_obj* n_) {
  r_ssize n = validate_n(n_);
  if (r_typeof(x) != R_TYPE_list) {
    return r_false;
  }
  if (n < 0) {
    return r_true;
  }
  return r_shared_lgl(r_length(x) == n);
}

r_obj* ffi_is_atomic(r_obj* x, r_obj* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_atomic(x, n));
}
r_obj* ffi_is_vector(r_obj* x, r_obj* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_vector(x, n));
}

r_obj* ffi_is_logical(r_obj* x, r_obj* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_logical(x, n));
}
r_obj* ffi_is_integer(r_obj* x, r_obj* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_integer(x, n, -1));
}
r_obj* ffi_is_double(r_obj* x, r_obj* n_, r_obj* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_double(x, n, finite));
}
r_obj* ffi_is_complex(r_obj* x, r_obj* n_, r_obj* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_complex(x, n, finite));
}
r_obj* ffi_is_integerish(r_obj* x, r_obj* n_, r_obj* finite_) {
  r_ssize n = validate_n(n_);
  int finite = validate_finite(finite_);
  return r_shared_lgl(r_is_integerish(x, n, finite));
}

static
enum option_bool as_option_bool(r_obj* x) {
  if (x == r_null) {
    return(OPTION_BOOL_null);
  }
  if (r_as_bool(x)) {
    return OPTION_BOOL_true;
  } else {
    return OPTION_BOOL_false;
  }
}

r_obj* ffi_is_character(r_obj* x,
                        r_obj* ffi_n,
                        r_obj* ffi_missing,
                        r_obj* ffi_empty) {
  r_ssize n = validate_n(ffi_n);

  enum option_bool missing = as_option_bool(ffi_missing);
  enum option_bool empty = as_option_bool(ffi_empty);

  return r_shared_lgl(is_character(x, n, missing, empty));
}
r_obj* ffi_is_raw(r_obj* x, r_obj* n_) {
  r_ssize n = validate_n(n_);
  return r_shared_lgl(r_is_raw(x, n));
}

r_obj* ffi_is_string(r_obj* x, r_obj* string, r_obj* empty) {
  if (r_typeof(x) != R_TYPE_character || r_length(x) != 1) {
    return r_false;
  }

  r_obj* value = r_chr_get(x, 0);

  if (value == r_globals.na_str) {
    return r_false;
  }

  if (string != r_null) {
    if (!ffi_is_string(string, r_null, r_null)) {
      r_abort("`string` must be `NULL` or a string.");
    }
    if (empty != r_null) {
      r_abort("Exactly one of `string` and `empty` must be supplied.");
    }

    bool matched = false;
    r_obj* const * p_string = r_chr_cbegin(string);
    r_ssize n = r_length(string);

    for (r_ssize i = 0; i < n; ++i) {
      if (p_string[i] == value) {
        matched = true;
        break;
      }
    }

    if (!matched) {
      return r_false;
    }
  }

  if (empty != r_null) {
    if (!r_is_bool(empty)) {
      r_abort("`empty` must be `NULL` or a logical value.");
    }

    bool c_empty = r_as_bool(empty);
    bool matched = c_empty == (value == r_strs.empty);
    return r_lgl(matched);
  }

  return r_true;
}

r_obj* ffi_vec_resize(r_obj* x, r_obj* n) {
  r_ssize n_ssize = r_arg_as_ssize(n, "n");

  switch (r_typeof(x)) {
  case R_TYPE_logical: return r_lgl_resize(x, n_ssize);
  case R_TYPE_integer: return r_int_resize(x, n_ssize);
  case R_TYPE_double: return r_dbl_resize(x, n_ssize);
  case R_TYPE_complex: return r_cpl_resize(x, n_ssize);
  case R_TYPE_raw: return r_raw_resize(x, n_ssize);
  case R_TYPE_character: return r_chr_resize(x, n_ssize);
  case R_TYPE_list: return r_list_resize(x, n_ssize);
  default: r_stop_unimplemented_type(r_typeof(x));
  }
}

r_obj* ffi_list_poke(r_obj* x, r_obj* i, r_obj* value) {
  r_list_poke(x, r_arg_as_ssize(i, "i"), value);
  return r_null;
}


// walk.c

static inline
r_obj* protect_missing(r_obj* x) {
  // FIXME: Include in `exec_` functions?
  if (x == r_missing_arg ||
      x == r_syms.unbound ||
      r_typeof(x) == R_TYPE_promise) {
    return r_expr_protect(x);
  } else {
    return x;
  }
}

r_obj* ffi_has_private_accessors(void) {
#ifdef RLANG_USE_PRIVATE_ACCESSORS
  return r_true;
#else
  return r_false;
#endif
}

#ifdef RLANG_USE_PRIVATE_ACCESSORS

// [[ register() ]]
r_obj* ffi_sexp_iterate(r_obj* x, r_obj* fn) {
  struct r_dyn_array* p_out = r_new_dyn_vector(R_TYPE_list, 256);
  KEEP(p_out->shelter);

  struct r_dict* p_dict = r_new_dict(1024);
  KEEP(p_dict->shelter);

  struct r_sexp_iterator* p_it = r_new_sexp_iterator(x);
  KEEP(p_it->shelter);

  for (int i = 0; r_sexp_next(p_it); ++i) {
    if (i % 100 == 0) {
      r_yield_interrupt();
    }

    if (p_it->x == r_envs.global) {
      p_it->skip_incoming = true;
      continue;
    }

    r_obj* x = p_it->x;
    enum r_type type = p_it->type;
    int depth = p_it->depth;
    r_obj* parent = p_it->parent;
    enum r_sexp_it_relation rel = p_it->rel;
    r_ssize i = p_it->i;
    enum r_sexp_it_direction dir = p_it->dir;

    if (dir == R_SEXP_IT_DIRECTION_incoming &&
        type == R_TYPE_environment &&
        !r_dict_put(p_dict, x, r_null)) {
      p_it->skip_incoming = true;
      continue;
    }

    struct r_pair args[] = {
      { r_sym("x"), KEEP(protect_missing(x)) },
      { r_sym("addr"), KEEP(r_str_as_character(r_obj_address(x))) },
      { r_sym("type"), KEEP(protect_missing(parent)) },
      { r_sym("depth"), KEEP(r_type_as_character(type)) },
      { r_sym("parent"), KEEP(r_int(depth)) },
      { r_sym("rel"), KEEP(r_chr(r_sexp_it_relation_as_c_string(rel))) },
      { r_sym("i"), KEEP(r_int(i + 1)) },
      { r_sym("dir"), KEEP(r_chr(r_sexp_it_direction_as_c_string(dir))) }
    };
    r_obj* out = KEEP(r_exec_mask_n(r_sym("fn"), fn,
                                    args,
                                    R_ARR_SIZEOF(args),
                                    r_envs.base));

    r_dyn_list_push_back(p_out, out);
    FREE(9);
  }

  FREE(3);
  return r_dyn_unwrap(p_out);
}

#endif
