#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include "rlang/export.h"

// Callable from other packages
extern SEXP rlang_new_dictionary(SEXP, SEXP, SEXP);
extern SEXP rlang_squash_if(SEXP, SEXPTYPE, bool (*is_spliceable)(SEXP), int);
extern bool rlang_is_clevel_spliceable(SEXP);

// Callable from this package
extern SEXP rlang_is_null(SEXP);
extern SEXP r_f_lhs(SEXP);
extern SEXP r_f_rhs(SEXP);
extern SEXP r_new_condition(SEXP, SEXP, SEXP);
extern SEXP rlang_mut_env_parent(SEXP, SEXP);
extern SEXP rlang_poke_type(SEXP, SEXP);
extern SEXP rlang_replace_na(SEXP, SEXP);
extern SEXP rlang_node_car(SEXP);
extern SEXP rlang_node_cdr(SEXP);
extern SEXP rlang_node_caar(SEXP);
extern SEXP rlang_node_cadr(SEXP);
extern SEXP rlang_node_cdar(SEXP);
extern SEXP rlang_node_cddr(SEXP);
extern SEXP rlang_missing_arg();
extern SEXP rlang_node_poke_car(SEXP, SEXP);
extern SEXP rlang_node_poke_cdr(SEXP, SEXP);
extern SEXP rlang_node_poke_caar(SEXP, SEXP);
extern SEXP rlang_node_poke_cadr(SEXP, SEXP);
extern SEXP rlang_node_poke_cdar(SEXP, SEXP);
extern SEXP rlang_node_poke_cddr(SEXP, SEXP);
extern SEXP rlang_new_node_(SEXP, SEXP);
extern SEXP rlang_duplicate(SEXP);
extern SEXP rlang_node_tag(SEXP);
extern SEXP rlang_node_poke_tag(SEXP, SEXP);
extern SEXP rlang_eval(SEXP, SEXP);
extern SEXP rlang_zap_attrs(SEXP);
extern SEXP rlang_get_attrs(SEXP);
extern SEXP rlang_set_attrs(SEXP, SEXP);
extern SEXP rlang_interp(SEXP, SEXP);
extern SEXP rlang_is_formulaish(SEXP, SEXP, SEXP);
extern SEXP rlang_is_reference(SEXP, SEXP);
extern SEXP rlang_sxp_address(SEXP);
extern SEXP rlang_length(SEXP);
extern SEXP rlang_new_dictionary(SEXP, SEXP, SEXP);
extern SEXP rlang_squash(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_symbol(SEXP);
extern SEXP rlang_symbol_to_character(SEXP);
extern SEXP rlang_tilde_eval(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_unescape_character(SEXP);
extern SEXP capture_arg(SEXP, SEXP);
extern SEXP rlang_capturearg(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_capturedots(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_new_call_node(SEXP, SEXP);
extern SEXP rlang_cnd_abort(SEXP, SEXP);
extern SEXP rlang_cnd_inform(SEXP, SEXP);
extern SEXP rlang_cnd_signal(SEXP, SEXP);
extern SEXP rlang_cnd_warn(SEXP, SEXP);
extern SEXP rlang_r_string(SEXP);
extern SEXP rlang_exprs_interp(SEXP, SEXP, SEXP);
extern SEXP rlang_quos_interp(SEXP, SEXP, SEXP);
extern SEXP rlang_dots_interp(SEXP, SEXP, SEXP);
extern SEXP r_new_formula(SEXP, SEXP, SEXP);
extern SEXP r_new_quosure(SEXP, SEXP);
extern SEXP rlang_forward_quosure(SEXP, SEXP);
extern SEXP rlang_poke_attributes(SEXP, SEXP);


// For unit tests
extern SEXP chr_prepend(SEXP, SEXP);
extern SEXP chr_append(SEXP, SEXP);
extern SEXP rlang_test_r_warn(SEXP);
extern SEXP rlang_on_exit(SEXP, SEXP);
extern SEXP rlang_test_is_special_op_sym(SEXP);
extern SEXP rlang_test_base_ns_get(SEXP);

static const R_CallMethodDef call_entries[] = {
  {"r_f_lhs",                   (DL_FUNC) &r_f_lhs, 1},
  {"r_f_rhs",                   (DL_FUNC) &r_f_rhs, 1},
  {"rlang_new_condition",       (DL_FUNC) &r_new_condition, 3},
  {"rlang_replace_na",          (DL_FUNC) &rlang_replace_na, 2},
  {"rlang_capturearg",          (DL_FUNC) &rlang_capturearg, 4},
  {"rlang_capturedots",         (DL_FUNC) &rlang_capturedots, 4},
  {"rlang_duplicate",           (DL_FUNC) &rlang_duplicate, 2},
  {"rlang_eval",                (DL_FUNC) &rlang_eval, 2},
  {"rlang_get_attrs",           (DL_FUNC) &rlang_get_attrs, 1},
  {"rlang_interp",              (DL_FUNC) &rlang_interp, 2},
  {"rlang_is_formulaish",       (DL_FUNC) &rlang_is_formulaish, 3},
  {"rlang_is_null",             (DL_FUNC) &rlang_is_null, 1},
  {"rlang_is_reference",        (DL_FUNC) &rlang_is_reference, 2},
  {"rlang_length",              (DL_FUNC) &rlang_length, 1},
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
  {"rlang_mut_env_parent",      (DL_FUNC) &rlang_mut_env_parent, 2},
  {"rlang_poke_type",           (DL_FUNC) &rlang_poke_type, 2},
  {"rlang_node_tag",            (DL_FUNC) &rlang_node_tag, 1},
  {"rlang_node_poke_tag",       (DL_FUNC) &rlang_node_poke_tag, 2},
  {"rlang_squash",              (DL_FUNC) &rlang_squash, 4},
  {"rlang_sxp_address",         (DL_FUNC) &rlang_sxp_address, 1},
  {"rlang_symbol",              (DL_FUNC) &rlang_symbol, 1},
  {"rlang_symbol_to_character", (DL_FUNC) &rlang_symbol_to_character, 1},
  {"rlang_tilde_eval",          (DL_FUNC) &rlang_tilde_eval, 5},
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
  {"rlang_r_string",            (DL_FUNC) &rlang_r_string, 1},
  {"rlang_exprs_interp",        (DL_FUNC) &rlang_exprs_interp, 3},
  {"rlang_quos_interp",         (DL_FUNC) &rlang_quos_interp, 3},
  {"rlang_dots_interp",         (DL_FUNC) &rlang_dots_interp, 3},
  {"rlang_new_formula",         (DL_FUNC) &r_new_formula, 3},
  {"rlang_new_quosure",         (DL_FUNC) &r_new_quosure, 2},
  {"rlang_forward_quosure",     (DL_FUNC) &rlang_forward_quosure, 2},
  {"rlang_poke_attributes",     (DL_FUNC) &rlang_poke_attributes, 2},
  {NULL, NULL, 0}
};

void R_init_rlang(DllInfo* dll) {
  // Register functions callable from other packages
  R_RegisterCCallable("rlang", "rlang_new_dictionary", (DL_FUNC) &rlang_new_dictionary);
  R_RegisterCCallable("rlang", "rlang_squash_if", (DL_FUNC) &rlang_squash_if);
  rlang_register_pointer("rlang", "rlang_test_is_spliceable", (DL_FUNC) &rlang_is_clevel_spliceable);

  // Register functions callable from this package
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
