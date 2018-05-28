#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <rlang.h>

// Callable from other packages
extern sexp* rlang_new_data_pronoun(sexp*, sexp*, sexp*);
extern sexp* r_squash_if(sexp*, enum r_type, bool (*is_spliceable)(sexp*), int);
extern bool rlang_is_clevel_spliceable(sexp*);
extern bool rlang_is_quosure(sexp*);

// Callable from this package
extern sexp* rlang_is_null(sexp*);
extern sexp* r_f_lhs(sexp*);
extern sexp* r_f_rhs(sexp*);
extern sexp* r_new_condition(sexp*, sexp*, sexp*);
extern sexp* r_env_clone(sexp*, sexp*);
extern sexp* rlang_env_unbind(sexp*, sexp*);
extern sexp* rlang_env_poke_parent(sexp*, sexp*);
extern sexp* rlang_env_frame(sexp* env);
extern sexp* rlang_env_hash_table(sexp* env);
extern sexp* rlang_poke_type(sexp*, sexp*);
extern sexp* rlang_replace_na(sexp*, sexp*);
extern sexp* rlang_node_car(sexp*);
extern sexp* rlang_node_cdr(sexp*);
extern sexp* rlang_node_caar(sexp*);
extern sexp* rlang_node_cadr(sexp*);
extern sexp* rlang_node_cdar(sexp*);
extern sexp* rlang_node_cddr(sexp*);
extern sexp* rlang_missing_arg();
extern sexp* rlang_node_poke_car(sexp*, sexp*);
extern sexp* rlang_node_poke_cdr(sexp*, sexp*);
extern sexp* rlang_node_poke_caar(sexp*, sexp*);
extern sexp* rlang_node_poke_cadr(sexp*, sexp*);
extern sexp* rlang_node_poke_cdar(sexp*, sexp*);
extern sexp* rlang_node_poke_cddr(sexp*, sexp*);
extern sexp* rlang_new_node_(sexp*, sexp*);
extern sexp* rlang_duplicate(sexp*, sexp*);
extern sexp* r_node_tree_clone(sexp*);
extern sexp* rlang_node_tag(sexp*);
extern sexp* rlang_node_poke_tag(sexp*, sexp*);
extern sexp* rlang_eval(sexp*, sexp*);
extern sexp* rlang_zap_attrs(sexp*);
extern sexp* rlang_get_attrs(sexp*);
extern sexp* rlang_set_attrs(sexp*, sexp*);
extern sexp* rlang_interp(sexp*, sexp*);
extern sexp* rlang_is_formulaish(sexp*, sexp*, sexp*);
extern sexp* rlang_is_reference(sexp*, sexp*);
extern sexp* rlang_sxp_address(sexp*);
extern sexp* rlang_length(sexp*);
extern sexp* rlang_true_length(sexp* x);
extern sexp* rlang_new_data_pronoun(sexp*, sexp*, sexp*);
extern sexp* rlang_squash(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_symbol(sexp*);
extern sexp* rlang_symbol_to_character(sexp*);
extern sexp* rlang_tilde_eval(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_unescape_character(sexp*);
extern sexp* rlang_capturearginfo(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_capturedots(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_new_call_node(sexp*, sexp*);
extern sexp* rlang_cnd_abort(sexp*, sexp*);
extern sexp* rlang_cnd_inform(sexp*, sexp*);
extern sexp* rlang_cnd_signal(sexp*, sexp*);
extern sexp* rlang_cnd_warn(sexp*, sexp*);
extern sexp* rlang_r_string(sexp*);
extern sexp* rlang_exprs_interp(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_quos_interp(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_dots_values(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_dots_list(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_dots_flat_list(sexp*, sexp*, sexp*, sexp*);
extern sexp* r_new_formula(sexp*, sexp*, sexp*);
extern sexp* rlang_new_quosure(sexp*, sexp*);
extern sexp* rlang_poke_attributes(sexp*, sexp*);
extern sexp* rlang_enexpr(sexp*, sexp*);
extern sexp* rlang_ensym(sexp*, sexp*);
extern sexp* rlang_enquo(sexp*, sexp*);
extern sexp* rlang_get_expression(sexp*, sexp*);
extern sexp* rlang_vec_coerce(sexp*, sexp*);
extern sexp* rlang_mark_object(sexp*);
extern sexp* rlang_unmark_object(sexp*);
extern sexp* rlang_quo_is_missing(sexp*);
extern sexp* rlang_quo_is_symbol(sexp*);
extern sexp* rlang_quo_is_call(sexp*);
extern sexp* rlang_quo_is_symbolic(sexp*);
extern sexp* rlang_quo_is_null(sexp*);
extern sexp* rlang_vec_poke_n(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_vec_poke_range(sexp*, sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_quo_get_expr(sexp*);
extern sexp* rlang_quo_set_expr(sexp*, sexp*);
extern sexp* rlang_quo_get_env(sexp*);
extern sexp* rlang_quo_set_env(sexp*, sexp*);
extern sexp* rlang_which_operator(sexp*);
extern sexp* rlang_new_data_mask(sexp*, sexp*, sexp*);
extern sexp* rlang_as_data_mask(sexp*, sexp*);
extern sexp* rlang_data_mask_clean(sexp*);
extern sexp* rlang_eval_tidy(sexp*, sexp*, sexp*);
extern sexp* rlang_as_data_pronoun(sexp*);

// Library initialisation defined below
sexp* rlang_library_load();
sexp* rlang_library_unload();

// For unit tests
extern sexp* chr_prepend(sexp*, sexp*);
extern sexp* chr_append(sexp*, sexp*);
extern sexp* rlang_test_r_warn(sexp*);
extern sexp* rlang_on_exit(sexp*, sexp*);
extern sexp* rlang_test_is_special_op_sym(sexp*);
extern sexp* rlang_test_base_ns_get(sexp*);
extern sexp* r_current_frame();
extern sexp* rlang_test_sys_frame(sexp*);
extern sexp* rlang_test_sys_call(sexp*);
extern sexp* new_tilde_thunk(sexp*, sexp*);

static const r_callable r_callables[] = {
  {"rlang_library_load",        (r_fn_ptr_t) &rlang_library_load, 0},
  {"rlang_library_unload",      (r_fn_ptr_t) &rlang_library_unload, 0},
  {"r_f_lhs",                   (r_fn_ptr_t) &r_f_lhs, 1},
  {"r_f_rhs",                   (r_fn_ptr_t) &r_f_rhs, 1},
  {"rlang_new_condition",       (r_fn_ptr_t) &r_new_condition, 3},
  {"rlang_replace_na",          (r_fn_ptr_t) &rlang_replace_na, 2},
  {"rlang_capturearginfo",      (r_fn_ptr_t) &rlang_capturearginfo, 4},
  {"rlang_capturedots",         (r_fn_ptr_t) &rlang_capturedots, 4},
  {"rlang_duplicate",           (r_fn_ptr_t) &rlang_duplicate, 2},
  {"rlang_node_tree_clone",     (r_fn_ptr_t) &r_node_tree_clone, 1},
  {"rlang_eval",                (r_fn_ptr_t) &rlang_eval, 2},
  {"rlang_get_attrs",           (r_fn_ptr_t) &rlang_get_attrs, 1},
  {"rlang_interp",              (r_fn_ptr_t) &rlang_interp, 2},
  {"rlang_is_formulaish",       (r_fn_ptr_t) &rlang_is_formulaish, 3},
  {"rlang_is_null",             (r_fn_ptr_t) &rlang_is_null, 1},
  {"rlang_is_reference",        (r_fn_ptr_t) &rlang_is_reference, 2},
  {"rlang_length",              (r_fn_ptr_t) &rlang_length, 1},
  {"rlang_true_length",         (r_fn_ptr_t) &rlang_true_length, 1},
  {"rlang_new_data_pronoun",    (r_fn_ptr_t) &rlang_new_data_pronoun, 3},
  {"rlang_set_attrs",           (r_fn_ptr_t) &rlang_set_attrs, 2},
  {"rlang_missing_arg",         (r_fn_ptr_t) &rlang_missing_arg, 0},
  {"rlang_node_car",            (r_fn_ptr_t) &rlang_node_car, 1},
  {"rlang_node_cdr",            (r_fn_ptr_t) &rlang_node_cdr, 1},
  {"rlang_node_caar",           (r_fn_ptr_t) &rlang_node_caar, 1},
  {"rlang_node_cadr",           (r_fn_ptr_t) &rlang_node_cadr, 1},
  {"rlang_node_cdar",           (r_fn_ptr_t) &rlang_node_cdar, 1},
  {"rlang_node_cddr",           (r_fn_ptr_t) &rlang_node_cddr, 1},
  {"rlang_node_poke_car",       (r_fn_ptr_t) &rlang_node_poke_car, 2},
  {"rlang_node_poke_cdr",       (r_fn_ptr_t) &rlang_node_poke_cdr, 2},
  {"rlang_node_poke_caar",      (r_fn_ptr_t) &rlang_node_poke_caar, 2},
  {"rlang_node_poke_cadr",      (r_fn_ptr_t) &rlang_node_poke_cadr, 2},
  {"rlang_node_poke_cdar",      (r_fn_ptr_t) &rlang_node_poke_cdar, 2},
  {"rlang_node_poke_cddr",      (r_fn_ptr_t) &rlang_node_poke_cddr, 2},
  {"rlang_new_node",            (r_fn_ptr_t) &rlang_new_node_, 2},
  {"rlang_env_clone",           (r_fn_ptr_t) &r_env_clone, 2},
  {"rlang_env_unbind",          (r_fn_ptr_t) &rlang_env_unbind, 3},
  {"rlang_env_poke_parent",     (r_fn_ptr_t) &rlang_env_poke_parent, 2},
  {"rlang_env_frame",           (r_fn_ptr_t) &rlang_env_frame, 1},
  {"rlang_env_hash_table",      (r_fn_ptr_t) &rlang_env_hash_table, 1},
  {"rlang_poke_type",           (r_fn_ptr_t) &rlang_poke_type, 2},
  {"rlang_mark_object",         (r_fn_ptr_t) &rlang_mark_object, 1},
  {"rlang_unmark_object",       (r_fn_ptr_t) &rlang_unmark_object, 1},
  {"rlang_node_tag",            (r_fn_ptr_t) &rlang_node_tag, 1},
  {"rlang_node_poke_tag",       (r_fn_ptr_t) &rlang_node_poke_tag, 2},
  {"rlang_squash",              (r_fn_ptr_t) &rlang_squash, 4},
  {"rlang_sxp_address",         (r_fn_ptr_t) &rlang_sxp_address, 1},
  {"rlang_symbol",              (r_fn_ptr_t) &rlang_symbol, 1},
  {"rlang_symbol_to_character", (r_fn_ptr_t) &rlang_symbol_to_character, 1},
  {"rlang_tilde_eval",          (r_fn_ptr_t) &rlang_tilde_eval, 5},
  {"rlang_unescape_character",  (r_fn_ptr_t) &rlang_unescape_character, 1},
  {"rlang_zap_attrs",           (r_fn_ptr_t) &rlang_zap_attrs, 1},
  {"rlang_new_call",            (r_fn_ptr_t) &rlang_new_call_node, 2},
  {"rlang_cnd_abort",           (r_fn_ptr_t) &rlang_cnd_abort, 2},
  {"rlang_cnd_inform",          (r_fn_ptr_t) &rlang_cnd_inform, 2},
  {"rlang_cnd_signal",          (r_fn_ptr_t) &rlang_cnd_signal, 2},
  {"rlang_cnd_warn",            (r_fn_ptr_t) &rlang_cnd_warn, 2},
  {"rlang_test_chr_prepend",    (r_fn_ptr_t) &chr_prepend, 2},
  {"rlang_test_chr_append",     (r_fn_ptr_t) &chr_append, 2},
  {"rlang_test_r_warn",         (r_fn_ptr_t) &rlang_test_r_warn, 1},
  {"rlang_test_r_on_exit",      (r_fn_ptr_t) &rlang_on_exit, 2},
  {"rlang_test_is_special_op_sym", (r_fn_ptr_t) &rlang_test_is_special_op_sym, 1},
  {"rlang_test_base_ns_get",    (r_fn_ptr_t) &rlang_test_base_ns_get, 1},
  {"rlang_test_current_frame",  (r_fn_ptr_t) &r_current_frame, 0},
  {"rlang_test_sys_frame",      (r_fn_ptr_t) &rlang_test_sys_frame, 1},
  {"rlang_test_sys_call",       (r_fn_ptr_t) &rlang_test_sys_call, 1},
  {"rlang_new_tilde_thunk",     (r_fn_ptr_t) &new_tilde_thunk, 2},
  {"rlang_r_string",            (r_fn_ptr_t) &rlang_r_string, 1},
  {"rlang_exprs_interp",        (r_fn_ptr_t) &rlang_exprs_interp, 4},
  {"rlang_quos_interp",         (r_fn_ptr_t) &rlang_quos_interp, 4},
  {"rlang_dots_values",         (r_fn_ptr_t) &rlang_dots_values, 4},
  {"rlang_dots_list",           (r_fn_ptr_t) &rlang_dots_list, 4},
  {"rlang_dots_flat_list",      (r_fn_ptr_t) &rlang_dots_flat_list, 4},
  {"rlang_new_formula",         (r_fn_ptr_t) &r_new_formula, 3},
  {"rlang_new_quosure",         (r_fn_ptr_t) &rlang_new_quosure, 2},
  {"rlang_poke_attributes",     (r_fn_ptr_t) &rlang_poke_attributes, 2},
  {"rlang_enexpr",              (r_fn_ptr_t) &rlang_enexpr, 2},
  {"rlang_ensym",               (r_fn_ptr_t) &rlang_ensym, 2},
  {"rlang_enquo",               (r_fn_ptr_t) &rlang_enquo, 2},
  {"rlang_get_expression",      (r_fn_ptr_t) &rlang_get_expression, 2},
  {"rlang_vec_coerce",          (r_fn_ptr_t) &rlang_vec_coerce, 2},
  {"rlang_quo_is_symbol",       (r_fn_ptr_t) &rlang_quo_is_symbol, 1},
  {"rlang_quo_is_call",         (r_fn_ptr_t) &rlang_quo_is_call, 1},
  {"rlang_quo_is_symbolic",     (r_fn_ptr_t) &rlang_quo_is_symbolic, 1},
  {"rlang_quo_is_missing",      (r_fn_ptr_t) &rlang_quo_is_missing, 1},
  {"rlang_quo_is_null",         (r_fn_ptr_t) &rlang_quo_is_null, 1},
  {"rlang_quo_get_expr",        (r_fn_ptr_t) &rlang_quo_get_expr, 1},
  {"rlang_quo_set_expr",        (r_fn_ptr_t) &rlang_quo_set_expr, 2},
  {"rlang_quo_get_env",         (r_fn_ptr_t) &rlang_quo_get_env, 1},
  {"rlang_quo_set_env",         (r_fn_ptr_t) &rlang_quo_set_env, 2},
  {"rlang_vec_poke_n",          (r_fn_ptr_t) &rlang_vec_poke_n, 5},
  {"rlang_vec_poke_range",      (r_fn_ptr_t) &rlang_vec_poke_range, 5},
  {"rlang_which_operator",      (r_fn_ptr_t) &rlang_which_operator, 1},
  {"rlang_call_has_precedence", (r_fn_ptr_t) &rlang_call_has_precedence, 3},
  {"rlang_new_data_mask",       (r_fn_ptr_t) &rlang_new_data_mask, 3},
  {"rlang_as_data_mask",        (r_fn_ptr_t) &rlang_as_data_mask, 2},
  {"rlang_data_mask_clean",     (r_fn_ptr_t) &rlang_data_mask_clean, 1},
  {"rlang_eval_tidy",           (r_fn_ptr_t) &rlang_eval_tidy, 3},
  {"rlang_as_data_pronoun",     (r_fn_ptr_t) &rlang_as_data_pronoun, 1},
  {NULL, NULL, 0}
};

void R_init_rlang(r_dll_info* dll) {
  /* r_register_c_callable("rlang", "rlang_new_data_pronoun", (r_fn_ptr_t) &rlang_new_dictionary); */
  r_register_c_callable("rlang", "rlang_squash_if", (r_fn_ptr_t) &r_squash_if);

  // The quosure functions are stable
  r_register_c_callable("rlang", "rlang_new_quosure", (r_fn_ptr_t) &rlang_new_quosure);
  r_register_c_callable("rlang", "rlang_is_quosure", (r_fn_ptr_t) &rlang_is_quosure);
  r_register_c_callable("rlang", "rlang_quo_get_expr", (r_fn_ptr_t) &rlang_quo_get_expr);
  r_register_c_callable("rlang", "rlang_quo_set_expr", (r_fn_ptr_t) &rlang_quo_set_expr);
  r_register_c_callable("rlang", "rlang_quo_get_env", (r_fn_ptr_t) &rlang_quo_get_env);
  r_register_c_callable("rlang", "rlang_quo_set_env", (r_fn_ptr_t) &rlang_quo_set_env);

  // The data mask functions are stable
  r_register_c_callable("rlang", "rlang_as_data_pronoun", (r_fn_ptr_t) &rlang_as_data_pronoun);
  r_register_c_callable("rlang", "rlang_as_data_mask", (r_fn_ptr_t) &rlang_as_data_mask);
  r_register_c_callable("rlang", "rlang_new_data_mask", (r_fn_ptr_t) &rlang_new_data_mask);

  // Experimental method for exporting C function pointers as actual R objects
  rlang_register_pointer("rlang", "rlang_test_is_spliceable", (r_fn_ptr_t) &rlang_is_clevel_spliceable);

  r_register_r_callables(dll, r_callables);
}


// From "../internal/internal.h"
void rlang_init_internal();

sexp* rlang_library_load() {
  r_init_library();
  rlang_init_internal();
  return r_null;
}

sexp* rlang_library_unload() {
  return r_null;
}
