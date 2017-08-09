#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include "export.h"

// Callable from other packages
extern SEXP rlang_new_dictionary(SEXP, SEXP, SEXP);
extern SEXP rlang_squash_if(SEXP, SEXPTYPE, bool (*is_spliceable)(SEXP), int);
extern bool is_clevel_spliceable(SEXP);

// Callable from this package
extern SEXP f_lhs_(SEXP);
extern SEXP f_rhs_(SEXP);
extern SEXP r_mut_env_parent(SEXP, SEXP);
extern SEXP rlang_replace_na(SEXP, SEXP);
extern SEXP rlang_car(SEXP);
extern SEXP rlang_cdr(SEXP);
extern SEXP rlang_caar(SEXP);
extern SEXP rlang_cadr(SEXP);
extern SEXP rlang_cdar(SEXP);
extern SEXP rlang_cddr(SEXP);
extern SEXP rlang_set_car(SEXP, SEXP);
extern SEXP rlang_set_cdr(SEXP, SEXP);
extern SEXP rlang_set_caar(SEXP, SEXP);
extern SEXP rlang_set_cadr(SEXP, SEXP);
extern SEXP rlang_set_cdar(SEXP, SEXP);
extern SEXP rlang_set_cddr(SEXP, SEXP);
extern SEXP rlang_cons(SEXP, SEXP);
extern SEXP rlang_duplicate(SEXP);
extern SEXP rlang_shallow_duplicate(SEXP);
extern SEXP rlang_tag(SEXP);
extern SEXP rlang_set_tag(SEXP);
extern SEXP rlang_eval(SEXP, SEXP);
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
extern SEXP rlang_unescape_character(SEXP);
extern SEXP capture_arg(SEXP, SEXP);
extern SEXP rlang_capturearg(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_capturedots(SEXP, SEXP, SEXP, SEXP);
extern SEXP rlang_new_language(SEXP, SEXP);

static const R_CallMethodDef call_entries[] = {
  {"f_lhs_",                    (DL_FUNC) &f_lhs_, 1},
  {"f_rhs_",                    (DL_FUNC) &f_rhs_, 1},
  {"rlang_replace_na",          (DL_FUNC) &rlang_replace_na, 2},
  {"rlang_caar",                (DL_FUNC) &rlang_caar, 1},
  {"rlang_cadr",                (DL_FUNC) &rlang_cadr, 1},
  {"rlang_capturearg",          (DL_FUNC) &rlang_capturearg, 4},
  {"rlang_capturedots",         (DL_FUNC) &rlang_capturedots, 4},
  {"rlang_car",                 (DL_FUNC) &rlang_car, 1},
  {"rlang_cdar",                (DL_FUNC) &rlang_cdar, 1},
  {"rlang_cddr",                (DL_FUNC) &rlang_cddr, 1},
  {"rlang_cdr",                 (DL_FUNC) &rlang_cdr, 1},
  {"rlang_cons",                (DL_FUNC) &rlang_cons, 2},
  {"rlang_duplicate",           (DL_FUNC) &rlang_duplicate, 1},
  {"rlang_eval",                (DL_FUNC) &rlang_eval, 2},
  {"rlang_get_attrs",           (DL_FUNC) &rlang_get_attrs, 1},
  {"rlang_interp",              (DL_FUNC) &rlang_interp, 3},
  {"rlang_is_formulaish",       (DL_FUNC) &rlang_is_formulaish, 3},
  {"rlang_is_reference",        (DL_FUNC) &rlang_is_reference, 2},
  {"rlang_length",              (DL_FUNC) &rlang_length, 1},
  {"rlang_new_dictionary",      (DL_FUNC) &rlang_new_dictionary, 3},
  {"rlang_set_attrs",           (DL_FUNC) &rlang_set_attrs, 2},
  {"rlang_set_caar",            (DL_FUNC) &rlang_set_caar, 2},
  {"rlang_set_cadr",            (DL_FUNC) &rlang_set_cadr, 2},
  {"rlang_set_car",             (DL_FUNC) &rlang_set_car, 2},
  {"rlang_set_cdar",            (DL_FUNC) &rlang_set_cdar, 2},
  {"rlang_set_cddr",            (DL_FUNC) &rlang_set_cddr, 2},
  {"rlang_set_cdr",             (DL_FUNC) &rlang_set_cdr, 2},
  {"rlang_mut_env_parent",      (DL_FUNC) &r_mut_env_parent, 2},
  {"rlang_set_tag",             (DL_FUNC) &rlang_set_tag, 2},
  {"rlang_shallow_duplicate",   (DL_FUNC) &rlang_shallow_duplicate, 1},
  {"rlang_squash",              (DL_FUNC) &rlang_squash, 4},
  {"rlang_sxp_address",         (DL_FUNC) &rlang_sxp_address, 1},
  {"rlang_symbol",              (DL_FUNC) &rlang_symbol, 1},
  {"rlang_symbol_to_character", (DL_FUNC) &rlang_symbol_to_character, 1},
  {"rlang_tag",                 (DL_FUNC) &rlang_tag, 1},
  {"rlang_unescape_character",  (DL_FUNC) &rlang_unescape_character, 1},
  {"rlang_zap_attrs",           (DL_FUNC) &rlang_zap_attrs, 1},
  {"rlang_new_language",        (DL_FUNC) &rlang_new_language, 2},
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
