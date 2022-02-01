#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <rlang.h>

// Library initialisation defined below
static r_obj* ffi_init_rlang(r_obj*);
static r_obj* ffi_fini_rlang();

// From version.c
extern r_obj* rlang_linked_version();

static const R_CallMethodDef r_callables[] = {
  {"ffi_alloc_data_frame",             (DL_FUNC) &ffi_alloc_data_frame, 3},
  {"ffi_as_data_mask",                 (DL_FUNC) &ffi_as_data_mask, 1},
  {"ffi_as_data_pronoun",              (DL_FUNC) &ffi_as_data_pronoun, 1},
  {"ffi_attrib",                       (DL_FUNC) &r_attrib, 1},
  {"ffi_c_tests",                      (DL_FUNC) &ffi_c_tests, 0},
  {"ffi_call_has_precedence",          (DL_FUNC) &ffi_call_has_precedence, 3},
  {"ffi_call_zap_inline",              (DL_FUNC) &ffi_call_zap_inline, 1},
  {"ffi_chr_get",                      (DL_FUNC) &ffi_chr_get, 2},
  {"ffi_chr_has_curly",                (DL_FUNC) &ffi_chr_has_curly, 1},
  {"ffi_cnd_signal",                   (DL_FUNC) &ffi_cnd_signal, 1},
  {"ffi_cnd_type",                     (DL_FUNC) &ffi_cnd_type, 1},
  {"ffi_compiled_by_gcc",              (DL_FUNC) &ffi_compiled_by_gcc, 0},
  {"ffi_data_mask_clean",              (DL_FUNC) &ffi_data_mask_clean, 1},
  {"ffi_data_pronoun_get",             (DL_FUNC) &ffi_data_pronoun_get, 3},
  {"ffi_dict_as_df_list",              (DL_FUNC) &ffi_dict_as_df_list, 1},
  {"ffi_dict_as_list",                 (DL_FUNC) &ffi_dict_as_list, 1},
  {"ffi_dict_del",                     (DL_FUNC) &ffi_dict_del, 2},
  {"ffi_dict_get",                     (DL_FUNC) &ffi_dict_get, 2},
  {"ffi_dict_has",                     (DL_FUNC) &ffi_dict_has, 2},
  {"ffi_dict_it_info",                 (DL_FUNC) &ffi_dict_it_info, 1},
  {"ffi_dict_next",                    (DL_FUNC) &ffi_dict_it_next, 1},
  {"ffi_dict_poke",                    (DL_FUNC) &ffi_dict_poke, 3},
  {"ffi_dict_put",                     (DL_FUNC) &ffi_dict_put, 3},
  {"ffi_dict_resize",                  (DL_FUNC) &ffi_dict_resize, 2},
  {"ffi_dots_flat_list",               (DL_FUNC) &ffi_dots_flat_list, 7},
  {"ffi_dots_list",                    (DL_FUNC) &ffi_dots_list, 7},
  {"ffi_dots_pairlist",                (DL_FUNC) &ffi_dots_pairlist, 7},
  {"ffi_duplicate",                    (DL_FUNC) &ffi_duplicate, 2},
  {"ffi_dyn_chr_get",                  (DL_FUNC) &ffi_dyn_chr_get, 2},
  {"ffi_dyn_chr_poke",                 (DL_FUNC) &ffi_dyn_chr_poke, 3},
  {"ffi_dyn_cpl_get",                  (DL_FUNC) &ffi_dyn_cpl_get, 2},
  {"ffi_dyn_cpl_poke",                 (DL_FUNC) &ffi_dyn_cpl_poke, 3},
  {"ffi_dyn_dbl_get",                  (DL_FUNC) &ffi_dyn_dbl_get, 2},
  {"ffi_dyn_dbl_poke",                 (DL_FUNC) &ffi_dyn_dbl_poke, 3},
  {"ffi_dyn_info",                     (DL_FUNC) &ffi_dyn_info, 1},
  {"ffi_dyn_int_get",                  (DL_FUNC) &ffi_dyn_int_get, 2},
  {"ffi_dyn_int_poke",                 (DL_FUNC) &ffi_dyn_int_poke, 3},
  {"ffi_dyn_lgl_get",                  (DL_FUNC) &ffi_dyn_lgl_get, 2},
  {"ffi_dyn_lgl_poke",                 (DL_FUNC) &ffi_dyn_lgl_poke, 3},
  {"ffi_dyn_list_get",                 (DL_FUNC) &ffi_dyn_list_get, 2},
  {"ffi_dyn_list_poke",                 (DL_FUNC) &ffi_dyn_list_poke, 3},
  {"ffi_dyn_pop_back",                 (DL_FUNC) &ffi_dyn_pop_back, 1},
  {"ffi_dyn_push_back",                (DL_FUNC) &ffi_dyn_push_back, 2},
  {"ffi_dyn_push_back_bool",           (DL_FUNC) &ffi_dyn_push_back_bool, 2},
  {"ffi_dyn_raw_get",                  (DL_FUNC) &ffi_dyn_raw_get, 2},
  {"ffi_dyn_raw_poke",                 (DL_FUNC) &ffi_dyn_raw_poke, 3},
  {"ffi_dyn_resize",                   (DL_FUNC) &ffi_dyn_resize, 2},
  {"ffi_dyn_unwrap",                   (DL_FUNC) &ffi_dyn_unwrap, 1},
  {"ffi_ellipsis_dots",                (DL_FUNC) &ffi_ellipsis_dots, 1},
  {"ffi_ellipsis_dots_used",           (DL_FUNC) &ffi_ellipsis_dots_used, 1},
  {"ffi_ellipsis_promise_forced",      (DL_FUNC) &ffi_ellipsis_promise_forced, 1},
  {"ffi_enexpr",                       (DL_FUNC) &ffi_enexpr, 2},
  {"ffi_enquo",                        (DL_FUNC) &ffi_enquo, 2},
  {"ffi_ensym",                        (DL_FUNC) &ffi_ensym, 2},
  {"ffi_env_bind",                     (DL_FUNC) &ffi_env_bind, 5},
  {"ffi_env_bind_list",                (DL_FUNC) &ffi_env_bind_list, 3},
  {"ffi_env_binding_types",            (DL_FUNC) &r_env_binding_types, 2},
  {"ffi_env_browse",                   (DL_FUNC) &ffi_env_browse, 2},
  {"ffi_env_clone",                    (DL_FUNC) &r_env_clone, 2},
  {"ffi_env_frame",                    (DL_FUNC) &ffi_env_frame, 1},
  {"ffi_env_get",                      (DL_FUNC) &ffi_env_get, 5},
  {"ffi_env_get_list",                 (DL_FUNC) &ffi_env_get_list, 5},
  {"ffi_env_has",                      (DL_FUNC) &ffi_env_has, 3},
  {"ffi_env_hash_table",               (DL_FUNC) &ffi_env_hash_table, 1},
  {"ffi_env_inherits",                 (DL_FUNC) &ffi_env_inherits, 2},
  {"ffi_env_is_browsed",               (DL_FUNC) &ffi_env_is_browsed, 1},
  {"ffi_env_poke",                     (DL_FUNC) &ffi_env_poke, 5},
  {"ffi_env_poke_parent",              (DL_FUNC) &ffi_env_poke_parent, 2},
  {"ffi_env_unbind",                   (DL_FUNC) &ffi_env_unbind, 3},
  {"ffi_env_unlock",                   (DL_FUNC) &ffi_env_unlock, 1},
  {"ffi_eval_top",                     (DL_FUNC) &ffi_eval_top, 2},
  {"ffi_exprs_interp",                 (DL_FUNC) &ffi_exprs_interp, 6},
  {"ffi_f_lhs",                        (DL_FUNC) &r_f_lhs, 1},
  {"ffi_f_rhs",                        (DL_FUNC) &r_f_rhs, 1},
  {"ffi_find_var",                     (DL_FUNC) &ffi_find_var, 2},
  {"ffi_find_var_in_frame",            (DL_FUNC) &ffi_find_var_in_frame, 2},
  {"ffi_fini_rlang",                   (DL_FUNC) &ffi_fini_rlang, 0},
  {"ffi_format_error_arg",             (DL_FUNC) &ffi_format_error_arg, 1},
  {"ffi_get_expression",               (DL_FUNC) &ffi_get_expression, 2},
  {"ffi_glue_is_here",                 (DL_FUNC) &ffi_glue_is_here, 0},
  {"ffi_has_local_precious_list",      (DL_FUNC) &ffi_has_local_precious_list, 0},
  {"ffi_hash",                         (DL_FUNC) &ffi_hash, 1},
  {"ffi_hash_file",                    (DL_FUNC) &ffi_hash_file, 1},
  {"ffi_hasher_init",                  (DL_FUNC) &ffi_hasher_init, 0},
  {"ffi_hasher_update",                (DL_FUNC) &ffi_hasher_update, 2},
  {"ffi_hasher_value",                 (DL_FUNC) &ffi_hasher_value, 1},
  {"ffi_init_r_library",               (DL_FUNC) &r_init_library, 1},
  {"ffi_init_rlang",                   (DL_FUNC) &ffi_init_rlang, 1},
  {"ffi_interp",                       (DL_FUNC) &ffi_interp, 2},
  {"ffi_interrupt",                    (DL_FUNC) &ffi_interrupt, 0},
  {"ffi_is_atomic",                    (DL_FUNC) &ffi_is_atomic, 2},
  {"ffi_is_call",                      (DL_FUNC) &ffi_is_call, 4},
  {"ffi_is_character",                 (DL_FUNC) &ffi_is_character, 4},
  {"ffi_is_closure",                   (DL_FUNC) &ffi_is_closure, 1},
  {"ffi_is_complex",                   (DL_FUNC) &ffi_is_complex, 3},
  {"ffi_is_data_mask",                 (DL_FUNC) &ffi_is_data_mask, 1},
  {"ffi_is_double",                    (DL_FUNC) &ffi_is_double, 3},
  {"ffi_is_finite",                    (DL_FUNC) &ffi_is_finite, 1},
  {"ffi_is_formula",                   (DL_FUNC) &ffi_is_formula, 3},
  {"ffi_is_function",                  (DL_FUNC) &ffi_is_function, 1},
  {"ffi_is_integer",                   (DL_FUNC) &ffi_is_integer, 2},
  {"ffi_is_integerish",                (DL_FUNC) &ffi_is_integerish, 3},
  {"ffi_is_list",                      (DL_FUNC) &ffi_is_list, 2},
  {"ffi_is_logical",                   (DL_FUNC) &ffi_is_logical, 2},
  {"ffi_is_primitive",                 (DL_FUNC) &ffi_is_primitive, 1},
  {"ffi_is_primitive_eager",           (DL_FUNC) &ffi_is_primitive_eager, 1},
  {"ffi_is_primitive_lazy",            (DL_FUNC) &ffi_is_primitive_lazy, 1},
  {"ffi_is_raw",                       (DL_FUNC) &ffi_is_raw, 2},
  {"ffi_is_reference",                 (DL_FUNC) &ffi_is_reference, 2},
  {"ffi_is_splice_box",                (DL_FUNC) &ffi_is_splice_box, 1},
  {"ffi_is_string",                    (DL_FUNC) &ffi_is_string, 3},
  {"ffi_is_vector",                    (DL_FUNC) &ffi_is_vector, 2},
  {"ffi_is_weakref",                   (DL_FUNC) &ffi_is_weakref, 1},
  {"ffi_length",                       (DL_FUNC) &ffi_length, 1},
  {"ffi_list_compact",                 (DL_FUNC) &r_list_compact, 1},
  {"ffi_list_poke",                    (DL_FUNC) &ffi_list_poke, 3},
  {"ffi_lof_arr_push_back",            (DL_FUNC) &ffi_lof_arr_push_back, 3},
  {"ffi_lof_info",                     (DL_FUNC) &ffi_lof_info, 1},
  {"ffi_lof_push_back",                (DL_FUNC) &ffi_lof_push_back, 1},
  {"ffi_lof_unwrap",                   (DL_FUNC) &ffi_lof_unwrap, 1},
  {"ffi_mark_object",                  (DL_FUNC) &ffi_mark_object, 1},
  {"ffi_missing_arg",                  (DL_FUNC) &ffi_missing_arg, 0},
  {"ffi_named",                        (DL_FUNC) &ffi_named, 2},
  {"ffi_names2",                       (DL_FUNC) &ffi_names2, 2},
  {"ffi_names_as_unique",              (DL_FUNC) &ffi_names_as_unique, 2},
  {"ffi_new_call",                     (DL_FUNC) &ffi_new_call_node, 2},
  {"ffi_new_condition",                (DL_FUNC) &ffi_new_condition, 3},
  {"ffi_new_data_mask",                (DL_FUNC) &ffi_new_data_mask, 2},
  {"ffi_new_dict",                     (DL_FUNC) &ffi_new_dict, 2},
  {"ffi_new_dict_iterator",            (DL_FUNC) &ffi_new_dict_iterator, 1},
  {"ffi_new_dyn_array",                (DL_FUNC) &ffi_new_dyn_array, 2},
  {"ffi_new_dyn_list_of",              (DL_FUNC) &ffi_new_dyn_list_of, 3},
  {"ffi_new_dyn_vector",               (DL_FUNC) &ffi_new_dyn_vector, 2},
  {"ffi_new_formula",                  (DL_FUNC) &r_new_formula, 3},
  {"ffi_new_function",                 (DL_FUNC) &ffi_new_function, 3},
  {"ffi_new_node",                     (DL_FUNC) &r_new_node, 2},
  {"ffi_new_quosure",                  (DL_FUNC) &ffi_new_quosure, 2},
  {"ffi_new_splice_box",               (DL_FUNC) &new_splice_box, 1},
  {"ffi_new_weakref",                  (DL_FUNC) &ffi_new_weakref, 4},
  {"ffi_nms_are_duplicated",           (DL_FUNC) &ffi_test_nms_are_duplicated, 2},
  {"ffi_node_caar",                    (DL_FUNC) &ffi_node_caar, 1},
  {"ffi_node_cadr",                    (DL_FUNC) &ffi_node_cadr, 1},
  {"ffi_node_car",                     (DL_FUNC) &ffi_node_car, 1},
  {"ffi_node_cdar",                    (DL_FUNC) &ffi_node_cdar, 1},
  {"ffi_node_cddr",                    (DL_FUNC) &ffi_node_cddr, 1},
  {"ffi_node_cdr",                     (DL_FUNC) &ffi_node_cdr, 1},
  {"ffi_node_poke_caar",               (DL_FUNC) &ffi_node_poke_caar, 2},
  {"ffi_node_poke_cadr",               (DL_FUNC) &ffi_node_poke_cadr, 2},
  {"ffi_node_poke_car",                (DL_FUNC) &ffi_node_poke_car, 2},
  {"ffi_node_poke_cdar",               (DL_FUNC) &ffi_node_poke_cdar, 2},
  {"ffi_node_poke_cddr",               (DL_FUNC) &ffi_node_poke_cddr, 2},
  {"ffi_node_poke_cdr",                (DL_FUNC) &ffi_node_poke_cdr, 2},
  {"ffi_node_poke_tag",                (DL_FUNC) &ffi_node_poke_tag, 2},
  {"ffi_node_tag",                     (DL_FUNC) &ffi_node_tag, 1},
  {"ffi_node_tree_clone",              (DL_FUNC) &r_node_tree_clone, 1},
  {"ffi_ns_registry_env",              (DL_FUNC) &ffi_ns_registry_env, 0},
  {"ffi_obj_address",                  (DL_FUNC) &ffi_obj_address, 1},
  {"ffi_pairlist_rev",                 (DL_FUNC) &r_pairlist_rev, 1},
  {"ffi_peek_srcref",                  (DL_FUNC) &ffi_peek_srcref, 0},
  {"ffi_poke_attrib",                  (DL_FUNC) &r_poke_attrib, 2},
  {"ffi_poke_type",                    (DL_FUNC) &ffi_poke_type, 2},
  {"ffi_precious_dict",                (DL_FUNC) &ffi_precious_dict, 0},
  {"ffi_preserve",                     (DL_FUNC) &ffi_preserve, 1},
  {"ffi_promise_env",                  (DL_FUNC) &ffi_promise_env, 2},
  {"ffi_promise_expr",                 (DL_FUNC) &ffi_promise_expr, 2},
  {"ffi_promise_value",                (DL_FUNC) &ffi_promise_value, 2},
  {"ffi_quo_get_env",                  (DL_FUNC) &ffi_quo_get_env, 1},
  {"ffi_quo_get_expr",                 (DL_FUNC) &ffi_quo_get_expr, 1},
  {"ffi_quo_is_call",                  (DL_FUNC) &ffi_quo_is_call, 1},
  {"ffi_quo_is_missing",               (DL_FUNC) &ffi_quo_is_missing, 1},
  {"ffi_quo_is_null",                  (DL_FUNC) &ffi_quo_is_null, 1},
  {"ffi_quo_is_symbol",                (DL_FUNC) &ffi_quo_is_symbol, 1},
  {"ffi_quo_is_symbolic",              (DL_FUNC) &ffi_quo_is_symbolic, 1},
  {"ffi_quo_set_env",                  (DL_FUNC) &ffi_quo_set_env, 2},
  {"ffi_quo_set_expr",                 (DL_FUNC) &ffi_quo_set_expr, 2},
  {"ffi_quos_interp",                  (DL_FUNC) &ffi_quos_interp, 6},
  {"ffi_r_string",                     (DL_FUNC) &ffi_r_string, 1},
  {"ffi_raw_deparse_str",              (DL_FUNC) &ffi_raw_deparse_str, 3},
  {"ffi_replace_na",                   (DL_FUNC) &ffi_replace_na, 2},
  {"ffi_run_c_test",                   (DL_FUNC) &ffi_run_c_test, 1},
  {"ffi_set_names",                    (DL_FUNC) &ffi_set_names, 4},
  {"ffi_sexp_iterate",                 (DL_FUNC) &ffi_sexp_iterate, 2},
  {"ffi_squash",                       (DL_FUNC) &ffi_squash, 4},
  {"ffi_sym_as_character",             (DL_FUNC) &ffi_sym_as_character, 1},
  {"ffi_symbol",                       (DL_FUNC) &ffi_symbol, 1},
  {"ffi_test_Rf_error",                (DL_FUNC) &ffi_test_Rf_error, 1},
  {"ffi_test_Rf_errorcall",            (DL_FUNC) &ffi_test_Rf_errorcall, 2},
  {"ffi_test_Rf_warning",              (DL_FUNC) &ffi_test_Rf_warning, 1},
  {"ffi_test_Rf_warningcall",          (DL_FUNC) &ffi_test_Rf_warningcall, 2},
  {"ffi_test_attrib_set",              (DL_FUNC) &r_attrib_set, 3},
  {"ffi_test_base_ns_get",             (DL_FUNC) &ffi_test_base_ns_get, 1},
  {"ffi_test_chr_append",              (DL_FUNC) &chr_append, 2},
  {"ffi_test_chr_prepend",             (DL_FUNC) &chr_prepend, 2},
  {"ffi_test_current_frame",           (DL_FUNC) &r_peek_frame, 0},
  {"ffi_test_lgl_sum",                 (DL_FUNC) &ffi_test_lgl_sum, 2},
  {"ffi_test_lgl_which",               (DL_FUNC) &ffi_test_lgl_which, 2},
  {"ffi_test_node_list_clone_until",   (DL_FUNC) &ffi_test_node_list_clone_until, 2},
  {"ffi_test_obj_encode_utf8",         (DL_FUNC) &obj_encode_utf8, 1},
  {"ffi_test_parse",                   (DL_FUNC) &ffi_test_parse, 1},
  {"ffi_test_parse_eval",              (DL_FUNC) &ffi_test_parse_eval, 2},
  {"ffi_test_r_on_exit",               (DL_FUNC) &r_on_exit, 2},
  {"ffi_test_r_warn",                  (DL_FUNC) &ffi_test_r_warn, 1},
  {"ffi_test_sys_call",                (DL_FUNC) &ffi_test_sys_call, 1},
  {"ffi_test_sys_frame",               (DL_FUNC) &ffi_test_sys_frame, 1},
  {"ffi_true_length",                  (DL_FUNC) &ffi_true_length, 1},
  {"ffi_unescape_character",           (DL_FUNC) &ffi_unescape_character, 1},
  {"ffi_unmark_object",                (DL_FUNC) &ffi_unmark_object, 1},
  {"ffi_unpreserve",                   (DL_FUNC) &ffi_unpreserve, 1},
  {"ffi_use_local_precious_list",      (DL_FUNC) &ffi_use_local_precious_list, 1},
  {"ffi_vec_alloc",                    (DL_FUNC) &ffi_vec_alloc, 2},
  {"ffi_vec_coerce",                   (DL_FUNC) &ffi_vec_coerce, 2},
  {"ffi_vec_poke_n",                   (DL_FUNC) &ffi_vec_poke_n, 5},
  {"ffi_vec_poke_range",               (DL_FUNC) &ffi_vec_poke_range, 5},
  {"ffi_vec_resize",                   (DL_FUNC) &ffi_vec_resize, 2},
  {"ffi_which_operator",               (DL_FUNC) &ffi_which_operator, 1},
  {"ffi_wref_key",                     (DL_FUNC) &ffi_wref_key, 1},
  {"ffi_wref_value",                   (DL_FUNC) &ffi_wref_value, 1},
  {"rlang_linked_version",             (DL_FUNC) &rlang_linked_version, 0},
  {NULL, NULL, 0}
};


static const R_ExternalMethodDef externals[] = {
  {"ffi_arg_match0",                    (DL_FUNC) &ffi_arg_match0, 4},
  {"ffi_call2",                         (DL_FUNC) &ffi_call2, 2},
  {"ffi_capturearginfo",                (DL_FUNC) &ffi_capturearginfo, 2},
  {"ffi_capturedots",                   (DL_FUNC) &ffi_capturedots, 1},
  {"ffi_dots_values",                   (DL_FUNC) &ffi_dots_values, 7},
  {"ffi_eval",                          (DL_FUNC) &ffi_eval, 2},
  {"ffi_eval_tidy",                     (DL_FUNC) &ffi_eval_tidy, 3},
  {"ffi_exec",                          (DL_FUNC) &ffi_exec, 2},
  {"ffi_tilde_eval",                    (DL_FUNC) &ffi_tilde_eval, 3},
  {"ffi_try_fetch",                     (DL_FUNC) &ffi_try_fetch, 1},
  {NULL, NULL, 0}
};


const struct r_test tests[] = {
  { "TRUE is TRUE",             &test_that_true_is_true },
  { "FALSE is FALSE",           &test_that_false_is_false },
  { NULL, NULL }
};


// From xxhash.h
extern uint64_t XXH3_64bits(const void*, size_t);

r_visible
void R_init_rlang(DllInfo* dll) {
  R_RegisterCCallable("rlang", "rlang_arg_match",           (DL_FUNC) &arg_match);
  R_RegisterCCallable("rlang", "rlang_as_data_mask_3.0.0",  (DL_FUNC) &ffi_as_data_mask);
  R_RegisterCCallable("rlang", "rlang_as_data_pronoun",     (DL_FUNC) &ffi_as_data_pronoun);
  R_RegisterCCallable("rlang", "rlang_env_unbind",          (DL_FUNC) &r_env_unbind);
  R_RegisterCCallable("rlang", "rlang_eval_tidy",           (DL_FUNC) &rlang_eval_tidy);
  R_RegisterCCallable("rlang", "rlang_format_error_arg",    (DL_FUNC) &rlang_format_error_arg);
  R_RegisterCCallable("rlang", "rlang_is_quosure",          (DL_FUNC) &is_quosure);
  R_RegisterCCallable("rlang", "rlang_names_as_unique",     (DL_FUNC) &names_as_unique);
  R_RegisterCCallable("rlang", "rlang_new_data_mask_3.0.0", (DL_FUNC) &ffi_new_data_mask);
  R_RegisterCCallable("rlang", "rlang_new_quosure",         (DL_FUNC) &ffi_new_quosure);
  R_RegisterCCallable("rlang", "rlang_quo_get_env",         (DL_FUNC) &ffi_quo_get_env);
  R_RegisterCCallable("rlang", "rlang_quo_get_expr",        (DL_FUNC) &ffi_quo_get_expr);
  R_RegisterCCallable("rlang", "rlang_quo_set_env",         (DL_FUNC) &ffi_quo_set_env);
  R_RegisterCCallable("rlang", "rlang_quo_set_expr",        (DL_FUNC) &ffi_quo_set_expr);
  R_RegisterCCallable("rlang", "rlang_stop_internal",       (DL_FUNC) &rlang_stop_internal);
  R_RegisterCCallable("rlang", "rlang_stop_internal2",      (DL_FUNC) &rlang_stop_internal2);

  r_obj* r_as_function(r_obj* x, const char* arg);
  R_RegisterCCallable("rlang", "rlang_as_function",         (DL_FUNC) &r_as_function);

  R_RegisterCCallable("rlang", "rlang_xxh3_64bits",         (DL_FUNC) &XXH3_64bits);

  // Maturing
  R_RegisterCCallable("rlang", "rlang_env_dots_list",       (DL_FUNC) &rlang_env_dots_list);
  R_RegisterCCallable("rlang", "rlang_env_dots_values",     (DL_FUNC) &rlang_env_dots_values);
  R_RegisterCCallable("rlang", "rlang_is_splice_box",       (DL_FUNC) &is_splice_box);
  R_RegisterCCallable("rlang", "rlang_obj_encode_utf8",     (DL_FUNC) &obj_encode_utf8);
  R_RegisterCCallable("rlang", "rlang_str_as_symbol",       (DL_FUNC) &r_str_as_symbol);
  R_RegisterCCallable("rlang", "rlang_sym_as_character",    (DL_FUNC) &ffi_sym_as_character);
  R_RegisterCCallable("rlang", "rlang_sym_as_string",       (DL_FUNC) &ffi_sym_as_string);
  R_RegisterCCallable("rlang", "rlang_unbox",               (DL_FUNC) &rlang_unbox);

  // Experimental
  R_RegisterCCallable("rlang", "rlang_squash_if",           (DL_FUNC) &r_squash_if);

  // Compatibility
  R_RegisterCCallable("rlang", "rlang_as_data_mask",        (DL_FUNC) &ffi_as_data_mask_compat);
  R_RegisterCCallable("rlang", "rlang_new_data_mask",       (DL_FUNC) &ffi_new_data_mask_compat);

  // Only for debugging - no stability guaranteed
  R_RegisterCCallable("rlang", "rlang_print_backtrace",     (DL_FUNC) &rlang_print_backtrace);

  R_registerRoutines(dll, NULL, r_callables, NULL, externals);
  R_useDynamicSymbols(dll, FALSE);
}


// From "../internal/internal.h"
void rlang_init_internal(r_obj* ns);

static
r_obj* ffi_init_rlang(r_obj* ns) {
  rlang_init_internal(ns);
  return r_null;
}

static
r_obj* ffi_fini_rlang() {
  return r_null;
}
