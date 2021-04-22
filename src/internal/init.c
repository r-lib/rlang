#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <rlang.h>

// Library initialisation defined below
r_obj* rlang_library_load(r_obj*);
r_obj* rlang_library_unload();

// From version.c
extern r_obj* rlang_linked_version();

static const R_CallMethodDef r_callables[] = {
  {"r_init_library",                    (DL_FUNC) &r_init_library, 1},
  {"rlang_library_load",                (DL_FUNC) &rlang_library_load, 1},
  {"rlang_library_unload",              (DL_FUNC) &rlang_library_unload, 0},
  {"r_f_lhs",                           (DL_FUNC) &r_f_lhs, 1},
  {"r_f_rhs",                           (DL_FUNC) &r_f_rhs, 1},
  {"rlang_new_condition",               (DL_FUNC) &r_new_condition, 3},
  {"rlang_replace_na",                  (DL_FUNC) &rlang_replace_na, 2},
  {"rlang_capturearginfo",              (DL_FUNC) &rlang_capturearginfo, 4},
  {"rlang_capturedots",                 (DL_FUNC) &rlang_capturedots, 4},
  {"rlang_duplicate",                   (DL_FUNC) &rlang_duplicate, 2},
  {"rlang_node_tree_clone",             (DL_FUNC) &r_node_tree_clone, 1},
  {"rlang_interp",                      (DL_FUNC) &rlang_interp, 2},
  {"rlang_is_function",                 (DL_FUNC) &rlang_is_function, 1},
  {"rlang_is_closure",                  (DL_FUNC) &rlang_is_closure, 1},
  {"rlang_is_primitive",                (DL_FUNC) &rlang_is_primitive, 1},
  {"rlang_is_primitive_eager",          (DL_FUNC) &rlang_is_primitive_eager, 1},
  {"rlang_is_primitive_lazy",           (DL_FUNC) &rlang_is_primitive_lazy, 1},
  {"ffi_is_formula",                    (DL_FUNC) &ffi_is_formula, 3},
  {"rlang_is_reference",                (DL_FUNC) &rlang_is_reference, 2},
  {"rlang_length",                      (DL_FUNC) &rlang_length, 1},
  {"rlang_true_length",                 (DL_FUNC) &rlang_true_length, 1},
  {"rlang_attrib",                      (DL_FUNC) &r_attrib, 1},
  {"rlang_poke_attrib",                 (DL_FUNC) &r_poke_attrib, 2},
  {"rlang_missing_arg",                 (DL_FUNC) &rlang_missing_arg, 0},
  {"rlang_node_car",                    (DL_FUNC) &rlang_node_car, 1},
  {"rlang_node_cdr",                    (DL_FUNC) &rlang_node_cdr, 1},
  {"rlang_node_caar",                   (DL_FUNC) &rlang_node_caar, 1},
  {"rlang_node_cadr",                   (DL_FUNC) &rlang_node_cadr, 1},
  {"rlang_node_cdar",                   (DL_FUNC) &rlang_node_cdar, 1},
  {"rlang_node_cddr",                   (DL_FUNC) &rlang_node_cddr, 1},
  {"rlang_node_poke_car",               (DL_FUNC) &rlang_node_poke_car, 2},
  {"rlang_node_poke_cdr",               (DL_FUNC) &rlang_node_poke_cdr, 2},
  {"rlang_node_poke_caar",              (DL_FUNC) &rlang_node_poke_caar, 2},
  {"rlang_node_poke_cadr",              (DL_FUNC) &rlang_node_poke_cadr, 2},
  {"rlang_node_poke_cdar",              (DL_FUNC) &rlang_node_poke_cdar, 2},
  {"rlang_node_poke_cddr",              (DL_FUNC) &rlang_node_poke_cddr, 2},
  {"rlang_new_node",                    (DL_FUNC) &r_new_node, 2},
  {"rlang_nms_are_duplicated",          (DL_FUNC) &rlang_test_nms_are_duplicated, 2},
  {"rlang_env_clone",                   (DL_FUNC) &r_env_clone, 2},
  {"rlang_env_unbind",                  (DL_FUNC) &rlang_env_unbind, 3},
  {"rlang_env_poke_parent",             (DL_FUNC) &rlang_env_poke_parent, 2},
  {"rlang_env_frame",                   (DL_FUNC) &rlang_env_frame, 1},
  {"rlang_env_hash_table",              (DL_FUNC) &rlang_env_hash_table, 1},
  {"rlang_poke_type",                   (DL_FUNC) &rlang_poke_type, 2},
  {"rlang_mark_object",                 (DL_FUNC) &rlang_mark_object, 1},
  {"rlang_promise_expr",                (DL_FUNC) &rlang_promise_expr, 2},
  {"rlang_promise_env",                 (DL_FUNC) &rlang_promise_env, 2},
  {"rlang_promise_value",               (DL_FUNC) &rlang_promise_value, 2},
  {"rlang_unmark_object",               (DL_FUNC) &rlang_unmark_object, 1},
  {"rlang_node_tag",                    (DL_FUNC) &rlang_node_tag, 1},
  {"rlang_node_poke_tag",               (DL_FUNC) &rlang_node_poke_tag, 2},
  {"rlang_squash",                      (DL_FUNC) &rlang_squash, 4},
  {"ffi_obj_address",                   (DL_FUNC) &ffi_obj_address, 1},
  {"rlang_symbol",                      (DL_FUNC) &rlang_symbol, 1},
  {"rlang_sym_as_character",            (DL_FUNC) &rlang_sym_as_character, 1},
  // No longer necessary but keep this around for a while in case
  // quosures ended up saved as RDS.
  {"rlang_tilde_eval",                  (DL_FUNC) &rlang_tilde_eval, 3},
  {"rlang_unescape_character",          (DL_FUNC) &rlang_unescape_character, 1},
  {"rlang_new_call",                    (DL_FUNC) &rlang_new_call_node, 2},
  {"rlang_cnd_signal",                  (DL_FUNC) &rlang_cnd_signal, 1},
  {"rlang_test_chr_prepend",            (DL_FUNC) &chr_prepend, 2},
  {"rlang_test_chr_append",             (DL_FUNC) &chr_append, 2},
  {"rlang_test_r_warn",                 (DL_FUNC) &rlang_test_r_warn, 1},
  {"rlang_test_r_on_exit",              (DL_FUNC) &rlang_on_exit, 2},
  {"rlang_test_base_ns_get",            (DL_FUNC) &rlang_test_base_ns_get, 1},
  {"rlang_test_current_frame",          (DL_FUNC) &r_peek_frame, 0},
  {"rlang_test_parse",                  (DL_FUNC) &rlang_test_parse, 1},
  {"rlang_test_parse_eval",             (DL_FUNC) &rlang_test_parse_eval, 2},
  {"rlang_test_node_list_clone_until",  (DL_FUNC) &rlang_test_node_list_clone_until, 2},
  {"rlang_test_attrib_set",             (DL_FUNC) &r_attrib_set, 3},
  {"rlang_test_sys_frame",              (DL_FUNC) &rlang_test_sys_frame, 1},
  {"rlang_test_sys_call",               (DL_FUNC) &rlang_test_sys_call, 1},
  {"rlang_test_Rf_warningcall",         (DL_FUNC) &rlang_test_Rf_warningcall, 2},
  {"rlang_test_Rf_errorcall",           (DL_FUNC) &rlang_test_Rf_errorcall, 2},
  {"rlang_test_lgl_sum",                (DL_FUNC) &rlang_test_lgl_sum, 2},
  {"rlang_test_lgl_which",              (DL_FUNC) &rlang_test_lgl_which, 2},
  {"rlang_r_string",                    (DL_FUNC) &rlang_r_string, 1},
  {"rlang_exprs_interp",                (DL_FUNC) &rlang_exprs_interp, 6},
  {"rlang_quos_interp",                 (DL_FUNC) &rlang_quos_interp, 6},
  {"rlang_dots_list",                   (DL_FUNC) &rlang_dots_list, 7},
  {"rlang_dots_flat_list",              (DL_FUNC) &rlang_dots_flat_list, 7},
  {"rlang_dots_pairlist",               (DL_FUNC) &rlang_dots_pairlist, 7},
  {"rlang_new_formula",                 (DL_FUNC) &r_new_formula, 3},
  {"rlang_new_quosure",                 (DL_FUNC) &rlang_new_quosure, 2},
  {"rlang_enexpr",                      (DL_FUNC) &rlang_enexpr, 2},
  {"rlang_ensym",                       (DL_FUNC) &rlang_ensym, 2},
  {"rlang_enquo",                       (DL_FUNC) &rlang_enquo, 2},
  {"rlang_get_expression",              (DL_FUNC) &rlang_get_expression, 2},
  {"rlang_vec_alloc",                   (DL_FUNC) &rlang_vec_alloc, 2},
  {"rlang_vec_coerce",                  (DL_FUNC) &rlang_vec_coerce, 2},
  {"rlang_quo_is_symbol",               (DL_FUNC) &rlang_quo_is_symbol, 1},
  {"rlang_quo_is_call",                 (DL_FUNC) &rlang_quo_is_call, 1},
  {"rlang_quo_is_symbolic",             (DL_FUNC) &rlang_quo_is_symbolic, 1},
  {"rlang_quo_is_missing",              (DL_FUNC) &rlang_quo_is_missing, 1},
  {"rlang_quo_is_null",                 (DL_FUNC) &rlang_quo_is_null, 1},
  {"rlang_quo_get_expr",                (DL_FUNC) &rlang_quo_get_expr, 1},
  {"rlang_quo_set_expr",                (DL_FUNC) &rlang_quo_set_expr, 2},
  {"rlang_quo_get_env",                 (DL_FUNC) &rlang_quo_get_env, 1},
  {"rlang_quo_set_env",                 (DL_FUNC) &rlang_quo_set_env, 2},
  {"rlang_vec_poke_n",                  (DL_FUNC) &rlang_vec_poke_n, 5},
  {"rlang_vec_poke_range",              (DL_FUNC) &rlang_vec_poke_range, 5},
  {"rlang_which_operator",              (DL_FUNC) &rlang_which_operator, 1},
  {"rlang_call_has_precedence",         (DL_FUNC) &rlang_call_has_precedence, 3},
  {"rlang_new_data_mask",               (DL_FUNC) &rlang_new_data_mask, 2},
  {"rlang_as_data_mask",                (DL_FUNC) &rlang_as_data_mask, 1},
  {"rlang_is_data_mask",                (DL_FUNC) &rlang_is_data_mask, 1},
  {"rlang_data_pronoun_get",            (DL_FUNC) &rlang_data_pronoun_get, 2},
  {"rlang_data_mask_clean",             (DL_FUNC) &rlang_data_mask_clean, 1},
  {"rlang_as_data_pronoun",             (DL_FUNC) &rlang_as_data_pronoun, 1},
  {"rlang_env_binding_types",           (DL_FUNC) &r_env_binding_types, 2},
  {"rlang_env_get",                     (DL_FUNC) &rlang_env_get, 4},
  {"rlang_env_get_list",                (DL_FUNC) &rlang_env_get_list, 4},
  {"rlang_env_unlock",                  (DL_FUNC) &rlang_env_unlock, 1},
  {"rlang_interrupt",                   (DL_FUNC) &rlang_interrupt, 0},
  {"rlang_is_list",                     (DL_FUNC) &rlang_is_list, 2},
  {"rlang_is_atomic",                   (DL_FUNC) &rlang_is_atomic, 2},
  {"rlang_is_vector",                   (DL_FUNC) &rlang_is_vector, 2},
  {"rlang_is_finite",                   (DL_FUNC) &rlang_is_finite, 1},
  {"rlang_is_logical",                  (DL_FUNC) &rlang_is_logical, 2},
  {"rlang_is_integer",                  (DL_FUNC) &rlang_is_integer, 2},
  {"rlang_is_double",                   (DL_FUNC) &rlang_is_double, 3},
  {"ffi_is_complex",                    (DL_FUNC) &ffi_is_complex, 3},
  {"rlang_is_integerish",               (DL_FUNC) &rlang_is_integerish, 3},
  {"rlang_is_character",                (DL_FUNC) &rlang_is_character, 2},
  {"rlang_is_raw",                      (DL_FUNC) &rlang_is_raw, 2},
  {"rlang_cnd_type",                    (DL_FUNC) &rlang_cnd_type, 1},
  {"rlang_env_inherits",                (DL_FUNC) &rlang_env_inherits, 2},
  {"rlang_eval_top",                    (DL_FUNC) &rlang_eval_top, 2},
  {"rlang_named",                       (DL_FUNC) &rlang_named, 2},
  {"rlang_pairlist_rev",                (DL_FUNC) &r_pairlist_rev, 1},
  {"rlang_new_splice_box",              (DL_FUNC) &rlang_new_splice_box, 1},
  {"rlang_is_splice_box",               (DL_FUNC) &rlang_is_splice_box, 1},
  {"rlang_new_function",                (DL_FUNC) &rlang_new_function, 3},
  {"rlang_is_string",                   (DL_FUNC) &rlang_is_string, 2},
  {"rlang_new_weakref",                 (DL_FUNC) &rlang_new_weakref, 4},
  {"rlang_wref_key",                    (DL_FUNC) &rlang_wref_key, 1},
  {"rlang_wref_value",                  (DL_FUNC) &rlang_wref_value, 1},
  {"rlang_is_weakref",                  (DL_FUNC) &rlang_is_weakref, 1},
  {"rlang_find_var",                    (DL_FUNC) &rlang_find_var, 2},
  {"rlang_env_bind_list",               (DL_FUNC) &rlang_env_bind_list, 3},
  {"rlang_glue_is_there",               (DL_FUNC) &rlang_glue_is_there, 0},
  {"rlang_linked_version",              (DL_FUNC) &rlang_linked_version, 0},
  {"rlang_names2",                      (DL_FUNC) &rlang_names2, 2},
  {"rlang_set_names",                   (DL_FUNC) &rlang_set_names, 4},
  {"rlang_chr_get",                     (DL_FUNC) &rlang_chr_get, 2},
  {"rlang_env_has",                     (DL_FUNC) &rlang_env_has, 3},
  {"rlang_env_poke",                    (DL_FUNC) &rlang_env_poke, 5},
  {"rlang_env_bind",                    (DL_FUNC) &rlang_env_bind, 5},
  {"rlang_raw_deparse_str",             (DL_FUNC) &rlang_raw_deparse_str, 3},
  {"rlang_env_browse",                  (DL_FUNC) &rlang_env_browse, 2},
  {"rlang_env_is_browsed",              (DL_FUNC) &rlang_env_is_browsed, 1},
  {"rlang_ns_registry_env",             (DL_FUNC) &rlang_ns_registry_env, 0},
  {"rlang_hash",                        (DL_FUNC) &rlang_hash, 1},
  {"rlang_new_dict",                    (DL_FUNC) &rlang_new_dict, 2},
  {"rlang_dict_put",                    (DL_FUNC) &rlang_dict_put, 3},
  {"rlang_dict_del",                    (DL_FUNC) &rlang_dict_del, 2},
  {"rlang_dict_has",                    (DL_FUNC) &rlang_dict_has, 2},
  {"rlang_dict_get",                    (DL_FUNC) &rlang_dict_get, 2},
  {"rlang_dict_resize",                 (DL_FUNC) &rlang_dict_resize, 2},
  {"c_ptr_precious_dict",               (DL_FUNC) &rlang_precious_dict, 0},
  {"c_ptr_preserve",                    (DL_FUNC) &rlang_preserve, 1},
  {"c_ptr_unpreserve",                  (DL_FUNC) &rlang_unpreserve, 1},
  {"c_ptr_alloc_data_frame",            (DL_FUNC) &rlang_alloc_data_frame, 3},
  {"c_ptr_list_compact",                (DL_FUNC) &r_list_compact, 1},
  {"c_ptr_vec_resize",                  (DL_FUNC) &rlang_vec_resize, 2},
  {"c_ptr_new_dyn_vector",              (DL_FUNC) &rlang_new_dyn_vector, 2},
  {"c_ptr_new_dyn_array",               (DL_FUNC) &rlang_new_dyn_array, 2},
  {"c_ptr_arr_unwrap",                  (DL_FUNC) &rlang_arr_unwrap, 1},
  {"c_ptr_arr_info",                    (DL_FUNC) &rlang_arr_info, 1},
  {"c_ptr_arr_push_back",               (DL_FUNC) &rlang_arr_push_back, 2},
  {"c_ptr_arr_push_back_bool",          (DL_FUNC) &rlang_arr_push_back_bool, 2},
  {"c_ptr_arr_pop_back",                (DL_FUNC) &rlang_arr_pop_back, 1},
  {"c_ptr_arr_resize",                  (DL_FUNC) &rlang_arr_resize, 2},
  {"c_ptr_list_poke",                   (DL_FUNC) &rlang_list_poke, 3},
  {"c_ptr_new_dict_iterator",           (DL_FUNC) &rlang_new_dict_iterator, 1},
  {"c_ptr_dict_it_info",                (DL_FUNC) &rlang_dict_it_info, 1},
  {"c_ptr_dict_next",                   (DL_FUNC) &rlang_dict_it_next, 1},
  {"c_ptr_dict_as_df_list",             (DL_FUNC) &rlang_dict_as_df_list, 1},
  {"c_ptr_dict_as_list",                (DL_FUNC) &rlang_dict_as_list, 1},
  {"ffi_new_dyn_list_of",               (DL_FUNC) &ffi_new_dyn_list_of, 3},
  {"ffi_lof_info",                      (DL_FUNC) &ffi_lof_info, 1},
  {"ffi_lof_unwrap",                    (DL_FUNC) &ffi_lof_unwrap, 1},
  {"ffi_lof_push_back",                 (DL_FUNC) &ffi_lof_push_back, 1},
  {"ffi_lof_arr_push_back",             (DL_FUNC) &ffi_lof_arr_push_back, 3},
  {"ffi_sexp_iterate",                  (DL_FUNC) &ffi_sexp_iterate, 2},
  {NULL, NULL, 0}
};

static const R_ExternalMethodDef externals[] = {
  {"ffi_arg_match0",                    (DL_FUNC) &ffi_arg_match0, 3},
  {"ffi_call2",                         (DL_FUNC) &ffi_call2, 2},
  {"ffi_capturearginfo",                (DL_FUNC) &ffi_capturearginfo, 2},
  {"ffi_capturedots",                   (DL_FUNC) &ffi_capturedots, 1},
  {"ffi_dots_values",                   (DL_FUNC) &ffi_dots_values, 7},
  {"ffi_eval",                          (DL_FUNC) &ffi_eval, 2},
  {"ffi_eval_tidy",                     (DL_FUNC) &ffi_eval_tidy, 3},
  {"ffi_exec",                          (DL_FUNC) &ffi_exec, 2},
  {"ffi_tilde_eval",                    (DL_FUNC) &ffi_tilde_eval, 3},
  {NULL, NULL, 0}
};

// From xxhash.h
extern uint64_t XXH3_64bits(const void*, size_t);

r_visible
void R_init_rlang(DllInfo* dll) {
  R_RegisterCCallable("rlang", "rlang_new_quosure", (DL_FUNC) &rlang_new_quosure);
  R_RegisterCCallable("rlang", "rlang_is_quosure", (DL_FUNC) &rlang_is_quosure);
  R_RegisterCCallable("rlang", "rlang_quo_get_expr", (DL_FUNC) &rlang_quo_get_expr);
  R_RegisterCCallable("rlang", "rlang_quo_set_expr", (DL_FUNC) &rlang_quo_set_expr);
  R_RegisterCCallable("rlang", "rlang_quo_get_env", (DL_FUNC) &rlang_quo_get_env);
  R_RegisterCCallable("rlang", "rlang_quo_set_env", (DL_FUNC) &rlang_quo_set_env);
  R_RegisterCCallable("rlang", "rlang_as_data_pronoun", (DL_FUNC) &rlang_as_data_pronoun);
  R_RegisterCCallable("rlang", "rlang_as_data_mask_3.0.0", (DL_FUNC) &rlang_as_data_mask);
  R_RegisterCCallable("rlang", "rlang_new_data_mask_3.0.0", (DL_FUNC) &rlang_new_data_mask);
  R_RegisterCCallable("rlang", "rlang_eval_tidy", (DL_FUNC) &rlang_eval_tidy);

  r_obj* r_as_function(r_obj* x, const char* arg);
  R_RegisterCCallable("rlang", "rlang_as_function", (DL_FUNC) &r_as_function);

  R_RegisterCCallable("rlang", "rlang_xxh3_64bits", (DL_FUNC) &XXH3_64bits);

  // Maturing
  R_RegisterCCallable("rlang", "rlang_is_splice_box", (DL_FUNC) &is_splice_box);
  R_RegisterCCallable("rlang", "rlang_unbox", (DL_FUNC) &rlang_unbox);
  R_RegisterCCallable("rlang", "rlang_env_dots_values", (DL_FUNC) &rlang_env_dots_values);
  R_RegisterCCallable("rlang", "rlang_env_dots_list", (DL_FUNC) &rlang_env_dots_list);
  R_RegisterCCallable("rlang", "rlang_sym_as_character", (DL_FUNC) &rlang_sym_as_character);
  R_RegisterCCallable("rlang", "rlang_str_as_symbol", (DL_FUNC) &r_str_as_symbol);

  // Experimental
  R_RegisterCCallable("rlang", "rlang_squash_if", (DL_FUNC) &r_squash_if);

  // Compatibility
  R_RegisterCCallable("rlang", "rlang_as_data_mask", (DL_FUNC) &rlang_as_data_mask_compat);
  R_RegisterCCallable("rlang", "rlang_new_data_mask", (DL_FUNC) &rlang_new_data_mask_compat);

  // Only for debugging - no stability guaranteed
  R_RegisterCCallable("rlang", "rlang_print_backtrace", (DL_FUNC) &rlang_print_backtrace);

  R_registerRoutines(dll, NULL, r_callables, NULL, externals);
  R_useDynamicSymbols(dll, FALSE);
}


// From "../internal/internal.h"
void rlang_init_internal(r_obj* ns);

r_obj* rlang_library_load(r_obj* ns) {
  rlang_init_internal(ns);
  return r_null;
}

r_obj* rlang_library_unload() {
  return r_null;
}
