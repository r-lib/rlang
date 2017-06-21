#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include "export.h"

// Callable from other packages
extern SEXP rlang_new_dictionary(SEXP, SEXP, SEXP);
extern SEXP rlang_squash_if(SEXP, SEXPTYPE, bool (*is_spliceable)(SEXP), int);
extern bool is_clevel_spliceable(SEXP);

// Callable from this package
extern SEXP rlang_is_null(SEXP);
extern SEXP r_f_lhs(SEXP);
extern SEXP r_f_rhs(SEXP);
extern SEXP new_condition(SEXP, SEXP, SEXP);
extern SEXP rlang_mut_env_parent(SEXP, SEXP);
extern SEXP rlang_replace_na(SEXP, SEXP);
extern SEXP r_node_car(SEXP);
extern SEXP r_node_cdr(SEXP);
extern SEXP r_node_caar(SEXP);
extern SEXP r_node_cadr(SEXP);
extern SEXP r_node_cdar(SEXP);
extern SEXP r_node_cddr(SEXP);
extern SEXP r_missing_arg();
extern SEXP r_mut_node_car(SEXP, SEXP);
extern SEXP r_mut_node_cdr(SEXP, SEXP);
extern SEXP r_mut_node_caar(SEXP, SEXP);
extern SEXP r_mut_node_cadr(SEXP, SEXP);
extern SEXP r_mut_node_cdar(SEXP, SEXP);
extern SEXP r_mut_node_cddr(SEXP, SEXP);
extern SEXP r_new_node_(SEXP, SEXP);
extern SEXP rlang_duplicate(SEXP);
extern SEXP r_node_tag(SEXP);
extern SEXP r_mut_node_tag(SEXP);
extern SEXP r_eval(SEXP, SEXP);
extern SEXP rlang_zap_attrs(SEXP);
extern SEXP rlang_get_attrs(SEXP);
extern SEXP rlang_set_attrs(SEXP, SEXP);
extern SEXP rlang_interp(SEXP, SEXP, SEXP);
extern SEXP rlang_is_formulaish(SEXP, SEXP, SEXP);
extern SEXP rlang_is_reference(SEXP, SEXP);
extern SEXP rlang_sxp_address(SEXP);
extern SEXP rlang_length(SEXP);
extern SEXP rlang_new_dictionary(SEXP, SEXP, SEXP);
extern SEXP rlang_squash(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_symbol(SEXP);
extern SEXP rlang_symbol_to_character(SEXP);
extern SEXP rlang_tilde_eval(SEXP, SEXP, SEXP);
extern SEXP rlang_unescape_character(SEXP);
extern SEXP capture_arg(SEXP, SEXP);
extern SEXP rlang_capturearg(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_capturedots(SEXP, SEXP, SEXP, SEXP);
extern SEXP r_new_language_(SEXP, SEXP);
extern SEXP rlang_cnd_signal(SEXP, SEXP);
extern SEXP rlang_cnd_signal_error(SEXP, SEXP);

// For unit tests
extern SEXP chr_prepend(SEXP, SEXP);
extern SEXP chr_append(SEXP, SEXP);
extern SEXP rlang_test_r_warn(SEXP);
extern SEXP r_on_exit(SEXP);

static const R_CallMethodDef call_entries[] = {
  {"r_f_lhs",                   (DL_FUNC) &r_f_lhs, 1},
  {"r_f_rhs",                   (DL_FUNC) &r_f_rhs, 1},
  {"rlang_new_condition",       (DL_FUNC) &new_condition, 3},
  {"rlang_replace_na",          (DL_FUNC) &rlang_replace_na, 2},
  {"r_node_caar",               (DL_FUNC) &r_node_caar, 1},
  {"r_node_cadr",               (DL_FUNC) &r_node_cadr, 1},
  {"rlang_capturearg",          (DL_FUNC) &rlang_capturearg, 4},
  {"rlang_capturedots",         (DL_FUNC) &rlang_capturedots, 4},
  {"r_node_car",                (DL_FUNC) &r_node_car, 1},
  {"r_node_cdar",               (DL_FUNC) &r_node_cdar, 1},
  {"r_node_cddr",               (DL_FUNC) &r_node_cddr, 1},
  {"r_node_cdr",                (DL_FUNC) &r_node_cdr, 1},
  {"r_new_node",                (DL_FUNC) &r_new_node_, 2},
  {"rlang_duplicate",           (DL_FUNC) &rlang_duplicate, 2},
  {"r_eval",                    (DL_FUNC) &r_eval, 2},
  {"rlang_get_attrs",           (DL_FUNC) &rlang_get_attrs, 1},
  {"rlang_interp",              (DL_FUNC) &rlang_interp, 3},
  {"rlang_is_formulaish",       (DL_FUNC) &rlang_is_formulaish, 3},
  {"rlang_is_null",             (DL_FUNC) &rlang_is_null, 1},
  {"rlang_is_reference",        (DL_FUNC) &rlang_is_reference, 2},
  {"rlang_length",              (DL_FUNC) &rlang_length, 1},
  {"rlang_new_dictionary",      (DL_FUNC) &rlang_new_dictionary, 3},
  {"rlang_set_attrs",           (DL_FUNC) &rlang_set_attrs, 2},
  {"r_missing_arg",             (DL_FUNC) &r_missing_arg, 0},
  {"r_mut_node_caar",           (DL_FUNC) &r_mut_node_caar, 2},
  {"r_mut_node_cadr",           (DL_FUNC) &r_mut_node_cadr, 2},
  {"r_mut_node_car",            (DL_FUNC) &r_mut_node_car, 2},
  {"r_mut_node_cdar",           (DL_FUNC) &r_mut_node_cdar, 2},
  {"r_mut_node_cddr",           (DL_FUNC) &r_mut_node_cddr, 2},
  {"r_mut_node_cdr",            (DL_FUNC) &r_mut_node_cdr, 2},
  {"rlang_mut_env_parent",      (DL_FUNC) &rlang_mut_env_parent, 2},
  {"r_mut_node_tag",            (DL_FUNC) &r_mut_node_tag, 2},
  {"rlang_squash",              (DL_FUNC) &rlang_squash, 4},
  {"rlang_sxp_address",         (DL_FUNC) &rlang_sxp_address, 1},
  {"rlang_symbol",              (DL_FUNC) &rlang_symbol, 1},
  {"rlang_symbol_to_character", (DL_FUNC) &rlang_symbol_to_character, 1},
  {"rlang_tilde_eval",          (DL_FUNC) &rlang_tilde_eval, 4},
  {"r_node_tag",                (DL_FUNC) &r_node_tag, 1},
  {"rlang_unescape_character",  (DL_FUNC) &rlang_unescape_character, 1},
  {"rlang_zap_attrs",           (DL_FUNC) &rlang_zap_attrs, 1},
  {"r_new_language",            (DL_FUNC) &r_new_language_, 2},
  {"rlang_cnd_signal",          (DL_FUNC) &rlang_cnd_signal, 2},
  {"rlang_cnd_signal_error",    (DL_FUNC) &rlang_cnd_signal_error, 2},
  {"rlang_test_chr_prepend",    (DL_FUNC) &chr_prepend, 2},
  {"rlang_test_chr_append",     (DL_FUNC) &chr_append, 2},
  {"rlang_test_r_warn",         (DL_FUNC) &rlang_test_r_warn, 1},
  {"rlang_test_r_on_exit",      (DL_FUNC) &r_on_exit, 2},
  {NULL, NULL, 0}
};

void R_init_rlang(DllInfo* dll) {
  // Register functions callable from other packages
  R_RegisterCCallable("rlang", "rlang_new_dictionary", (DL_FUNC) &rlang_new_dictionary);
  R_RegisterCCallable("rlang", "rlang_squash_if", (DL_FUNC) &rlang_squash_if);
  rlang_register_pointer("rlang", "test_is_spliceable", (DL_FUNC) &is_clevel_spliceable);

  // Register functions callable from this package
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
