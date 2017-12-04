#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include "rlang/rlang.h"
#include "rlang/export.h"

// Callable from other packages
extern sexp* rlang_new_dictionary(sexp*, sexp*, sexp*);
extern sexp* r_squash_if(sexp*, enum r_type, bool (*is_spliceable)(sexp*), int);
extern bool rlang_is_clevel_spliceable(sexp*);

// Callable from this package
extern sexp* rlang_is_null(sexp*);
extern sexp* r_f_lhs(sexp*);
extern sexp* r_f_rhs(sexp*);
extern sexp* r_new_condition(sexp*, sexp*, sexp*);
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
extern sexp* rlang_duplicate(sexp*);
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
extern sexp* rlang_new_dictionary(sexp*, sexp*, sexp*);
extern sexp* rlang_squash(sexp*, sexp*, sexp*, sexp*);
extern sexp* rlang_symbol(sexp*);
extern sexp* rlang_symbol_to_character(sexp*);
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
extern sexp* r_new_quosure(sexp*, sexp*);
extern sexp* rlang_poke_attributes(sexp*, sexp*);
extern sexp* rlang_enexpr(sexp*, sexp*);
extern sexp* rlang_enquo(sexp*, sexp*);
extern sexp* r_get_expression(sexp*, sexp*);
extern sexp* rlang_vec_coerce(sexp*, sexp*);
extern sexp* rlang_mark_object(sexp* x);
extern sexp* rlang_unmark_object(sexp* x);
extern sexp* rlang_quo_eval(sexp*, sexp*);
extern sexp* rlang_quo_is_missing(sexp* quo);
extern sexp* rlang_quo_is_symbol(sexp* quo);
extern sexp* rlang_quo_is_call(sexp* quo);
extern sexp* rlang_quo_is_symbolic(sexp* quo);
extern sexp* rlang_quo_is_null(sexp* quo);
extern sexp* rlang_new_overscope(sexp*, sexp*, sexp*);

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

static const R_CallMethodDef call_entries[] = {
  {"r_f_lhs",                   (DL_FUNC) &r_f_lhs, 1},
  {"r_f_rhs",                   (DL_FUNC) &r_f_rhs, 1},
  {"rlang_new_condition",       (DL_FUNC) &r_new_condition, 3},
  {"rlang_replace_na",          (DL_FUNC) &rlang_replace_na, 2},
  {"rlang_capturearginfo",      (DL_FUNC) &rlang_capturearginfo, 4},
  {"rlang_capturedots",         (DL_FUNC) &rlang_capturedots, 4},
  {"rlang_duplicate",           (DL_FUNC) &rlang_duplicate, 2},
  {"rlang_node_tree_clone",     (DL_FUNC) &r_node_tree_clone, 1},
  {"rlang_eval",                (DL_FUNC) &rlang_eval, 2},
  {"rlang_get_attrs",           (DL_FUNC) &rlang_get_attrs, 1},
  {"rlang_interp",              (DL_FUNC) &rlang_interp, 2},
  {"rlang_is_formulaish",       (DL_FUNC) &rlang_is_formulaish, 3},
  {"rlang_is_null",             (DL_FUNC) &rlang_is_null, 1},
  {"rlang_is_reference",        (DL_FUNC) &rlang_is_reference, 2},
  {"rlang_length",              (DL_FUNC) &rlang_length, 1},
  {"rlang_true_length",         (DL_FUNC) &rlang_true_length, 1},
  {"rlang_new_dictionary",      (DL_FUNC) &rlang_new_dictionary, 3},
  {"rlang_set_attrs",           (DL_FUNC) &rlang_set_attrs, 2},
  {"rlang_missing_arg",         (DL_FUNC) &rlang_missing_arg, 0},
  {"rlang_node_car",            (DL_FUNC) &rlang_node_car, 1},
  {"rlang_node_cdr",            (DL_FUNC) &rlang_node_cdr, 1},
  {"rlang_node_caar",           (DL_FUNC) &rlang_node_caar, 1},
  {"rlang_node_cadr",           (DL_FUNC) &rlang_node_cadr, 1},
  {"rlang_node_cdar",           (DL_FUNC) &rlang_node_cdar, 1},
  {"rlang_node_cddr",           (DL_FUNC) &rlang_node_cddr, 1},
  {"rlang_node_poke_car",       (DL_FUNC) &rlang_node_poke_car, 2},
  {"rlang_node_poke_cdr",       (DL_FUNC) &rlang_node_poke_cdr, 2},
  {"rlang_node_poke_caar",      (DL_FUNC) &rlang_node_poke_caar, 2},
  {"rlang_node_poke_cadr",      (DL_FUNC) &rlang_node_poke_cadr, 2},
  {"rlang_node_poke_cdar",      (DL_FUNC) &rlang_node_poke_cdar, 2},
  {"rlang_node_poke_cddr",      (DL_FUNC) &rlang_node_poke_cddr, 2},
  {"rlang_new_node",            (DL_FUNC) &rlang_new_node_, 2},
  {"rlang_env_poke_parent",     (DL_FUNC) &rlang_env_poke_parent, 2},
  {"rlang_env_frame",           (DL_FUNC) &rlang_env_frame, 1},
  {"rlang_env_hash_table",      (DL_FUNC) &rlang_env_hash_table, 1},
  {"rlang_poke_type",           (DL_FUNC) &rlang_poke_type, 2},
  {"rlang_mark_object",         (DL_FUNC) &rlang_mark_object, 1},
  {"rlang_unmark_object",       (DL_FUNC) &rlang_unmark_object, 1},
  {"rlang_node_tag",            (DL_FUNC) &rlang_node_tag, 1},
  {"rlang_node_poke_tag",       (DL_FUNC) &rlang_node_poke_tag, 2},
  {"rlang_squash",              (DL_FUNC) &rlang_squash, 4},
  {"rlang_sxp_address",         (DL_FUNC) &rlang_sxp_address, 1},
  {"rlang_symbol",              (DL_FUNC) &rlang_symbol, 1},
  {"rlang_symbol_to_character", (DL_FUNC) &rlang_symbol_to_character, 1},
  {"rlang_unescape_character",  (DL_FUNC) &rlang_unescape_character, 1},
  {"rlang_zap_attrs",           (DL_FUNC) &rlang_zap_attrs, 1},
  {"r_new_language",            (DL_FUNC) &rlang_new_call_node, 2},
  {"rlang_cnd_abort",           (DL_FUNC) &rlang_cnd_abort, 2},
  {"rlang_cnd_inform",          (DL_FUNC) &rlang_cnd_inform, 2},
  {"rlang_cnd_signal",          (DL_FUNC) &rlang_cnd_signal, 2},
  {"rlang_cnd_warn",            (DL_FUNC) &rlang_cnd_warn, 2},
  {"rlang_test_chr_prepend",    (DL_FUNC) &chr_prepend, 2},
  {"rlang_test_chr_append",     (DL_FUNC) &chr_append, 2},
  {"rlang_test_r_warn",         (DL_FUNC) &rlang_test_r_warn, 1},
  {"rlang_test_r_on_exit",      (DL_FUNC) &rlang_on_exit, 2},
  {"rlang_test_is_special_op_sym", (DL_FUNC) &rlang_test_is_special_op_sym, 1},
  {"rlang_test_base_ns_get",    (DL_FUNC) &rlang_test_base_ns_get, 1},
  {"rlang_test_current_frame",  (DL_FUNC) &r_current_frame, 0},
  {"rlang_test_sys_frame",      (DL_FUNC) &rlang_test_sys_frame, 1},
  {"rlang_test_sys_call",       (DL_FUNC) &rlang_test_sys_call, 1},
  {"rlang_r_string",            (DL_FUNC) &rlang_r_string, 1},
  {"rlang_exprs_interp",        (DL_FUNC) &rlang_exprs_interp, 4},
  {"rlang_quos_interp",         (DL_FUNC) &rlang_quos_interp, 4},
  {"rlang_dots_values",         (DL_FUNC) &rlang_dots_values, 4},
  {"rlang_dots_list",           (DL_FUNC) &rlang_dots_list, 4},
  {"rlang_dots_flat_list",      (DL_FUNC) &rlang_dots_flat_list, 4},
  {"rlang_new_formula",         (DL_FUNC) &r_new_formula, 3},
  {"rlang_new_quosure",         (DL_FUNC) &r_new_quosure, 2},
  {"rlang_poke_attributes",     (DL_FUNC) &rlang_poke_attributes, 2},
  {"rlang_enexpr",              (DL_FUNC) &rlang_enexpr, 2},
  {"rlang_enquo",               (DL_FUNC) &rlang_enquo, 2},
  {"rlang_get_expression",      (DL_FUNC) &r_get_expression, 2},
  {"rlang_vec_coerce",          (DL_FUNC) &rlang_vec_coerce, 2},
  {"rlang_quo_eval",            (DL_FUNC) &rlang_quo_eval, 2},
  {"rlang_quo_is_symbol",       (DL_FUNC) &rlang_quo_is_symbol, 1},
  {"rlang_quo_is_call",         (DL_FUNC) &rlang_quo_is_call, 1},
  {"rlang_quo_is_symbolic",     (DL_FUNC) &rlang_quo_is_symbolic, 1},
  {"rlang_quo_is_missing",      (DL_FUNC) &rlang_quo_is_missing, 1},
  {"rlang_quo_is_null",         (DL_FUNC) &rlang_quo_is_null, 1},
  {"rlang_new_overscope",       (DL_FUNC) &rlang_new_overscope, 3},
  {"rlang_library_load",        (DL_FUNC) &rlang_library_load, 0},
  {"rlang_library_unload",      (DL_FUNC) &rlang_library_unload, 0},
  {NULL, NULL, 0}
};

void R_init_rlang(DllInfo* dll) {
  // Register functions callable from other packages
  R_RegisterCCallable("rlang", "rlang_new_dictionary", (DL_FUNC) &rlang_new_dictionary);
  R_RegisterCCallable("rlang", "rlang_squash_if", (DL_FUNC) &r_squash_if);
  rlang_register_pointer("rlang", "rlang_test_is_spliceable", (DL_FUNC) &rlang_is_clevel_spliceable);

  // Register functions callable from this package
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

sexp* rlang_library_load() {
  return r_null;
}
sexp* rlang_library_unload() {
  return r_null;
}
