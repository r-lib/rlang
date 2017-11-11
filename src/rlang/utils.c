#include "rlang.h"

bool is_character(SEXP x) {
  return r_kind(x) == STRSXP;
}
bool is_str_empty(SEXP str) {
  const char* c_str = CHAR(str);
  return strcmp(c_str, "") == 0;
}

SEXP names(SEXP x) {
  return Rf_getAttrib(x, R_NamesSymbol);
}
bool has_name_at(SEXP x, r_size_t i) {
  SEXP nms = names(x);
  return is_character(nms) && !is_str_empty(STRING_ELT(nms, i));
}
SEXP set_names(SEXP x, SEXP nms) {
  return Rf_setAttrib(x, R_NamesSymbol, nms);
}

int is_symbolic(SEXP x) {
  return r_kind(x) == LANGSXP || r_kind(x) == SYMSXP;
}

SEXP rlang_length(SEXP x) {
  return Rf_ScalarInteger(r_length(x));
}

int is_true(SEXP x) {
  if (r_kind(x) != LGLSXP || r_length(x) != 1)
    r_abort("`x` must be a boolean");

  int value = LOGICAL(x)[0];
  return value == NA_LOGICAL ? 0 : value;
}

// Formulas --------------------------------------------------------------------

SEXP make_formula1(SEXP rhs, SEXP env) {
  SEXP f = KEEP(Rf_lang2(r_sym("~"), rhs));
  Rf_setAttrib(f, R_ClassSymbol, Rf_mkString("formula"));
  Rf_setAttrib(f, r_sym(".Environment"), env);

  FREE(1);
  return f;
}

SEXP pkg_obj(SEXP env, const char* name) {
  SEXP obj = r_env_get(env, r_sym(name));

  // Can be a promise to a lazyLoadDBfetch() call
  if (r_kind(obj) == PROMSXP)
    obj = r_eval(obj, r_empty_env);

  return obj;
}
SEXP rlang_obj(const char* name) {
  return pkg_obj(r_ns_env("rlang"), name);
}
SEXP base_obj(const char* name) {
  return pkg_obj(r_base_env, name);
}

const char* kind_c_str(SEXPTYPE kind) {
  SEXP str = Rf_type2str(kind);
  return CHAR(str);
}

bool is_empty(SEXP x) {
  return r_length(x) == 0;
}

SEXP rlang_new_dictionary(SEXP x, SEXP lookup_msg, SEXP read_only) {
  SEXP dict = KEEP(r_new_vector(VECSXP, 3));

  SET_VECTOR_ELT(dict, 0, x);
  SET_VECTOR_ELT(dict, 2, read_only);

  if (lookup_msg == r_null)
    SET_VECTOR_ELT(dict, 1, Rf_mkString("Object `%s` not found in data"));
  else
    SET_VECTOR_ELT(dict, 1, lookup_msg);

  static SEXP nms = NULL;
  if (!nms) {
    nms = r_new_vector(STRSXP, 3);
    R_PreserveObject(nms);
    SET_STRING_ELT(nms, 0, Rf_mkChar("src"));
    SET_STRING_ELT(nms, 1, Rf_mkChar("lookup_msg"));
    SET_STRING_ELT(nms, 2, Rf_mkChar("read_only"));
  }
  static SEXP s3 = NULL;
  if (!s3) {
    s3 = Rf_mkString("dictionary");
    R_PreserveObject(s3);
  }

  Rf_setAttrib(dict, R_ClassSymbol, s3);
  Rf_setAttrib(dict, R_NamesSymbol, nms);

  FREE(1);
  return dict;
}
