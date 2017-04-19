#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

bool is_character(SEXP x) {
  return TYPEOF(x) == STRSXP;
}
bool is_str_empty(SEXP str) {
  const char* c_str = CHAR(str);
  return strcmp(c_str, "") == 0;
}

SEXP names(SEXP x) {
  return Rf_getAttrib(x, R_NamesSymbol);
}
bool has_name_at(SEXP x, R_len_t i) {
  SEXP nms = names(x);
  return is_character(nms) && !is_str_empty(STRING_ELT(nms, i));
}
SEXP set_names(SEXP x, SEXP nms) {
  return Rf_setAttrib(x, R_NamesSymbol, nms);
}

bool is_object(SEXP x) {
  return OBJECT(x) != 0;
}
bool is_atomic(SEXP x) {
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return true;
  default:
    return false;
  }
}
bool is_scalar_atomic(SEXP x) {
  return Rf_length(x) == 1 && is_atomic(x);
}
bool is_list(SEXP x) {
  return TYPEOF(x) == VECSXP;
}
bool is_vector(SEXP x) {
  switch(TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return true;
  default:
    return false;
  }
}
bool is_null(SEXP x) {
  return x == R_NilValue;
}

int is_sym(SEXP x, const char* string) {
  if (TYPEOF(x) != SYMSXP)
    return false;
  else
    return strcmp(CHAR(PRINTNAME(x)), string) == 0;
}

int is_symbolic(SEXP x) {
  return TYPEOF(x) == LANGSXP || TYPEOF(x) == SYMSXP;
}

bool is_lang(SEXP x, const char* f) {
  if (!is_symbolic(x) && TYPEOF(x) != LISTSXP)
    return false;

  SEXP fun = CAR(x);
  return is_sym(fun, f);
}

int is_prefixed_call(SEXP x, int (*sym_predicate)(SEXP)) {
  if (TYPEOF(x) != LANGSXP)
    return 0;

  SEXP head = CAR(x);
  if (!(is_lang(head, "$") ||
        is_lang(head, "@") ||
        is_lang(head, "::") ||
        is_lang(head, ":::")))
    return 0;

  if (sym_predicate == NULL)
    return 1;

  SEXP args = CDAR(x);
  SEXP sym = CADR(args);
  return sym_predicate(sym);
}

int is_any_call(SEXP x, int (*sym_predicate)(SEXP)) {
  if (TYPEOF(x) != LANGSXP)
    return false;
  else
    return sym_predicate(CAR(x)) || is_prefixed_call(x, sym_predicate);
}

int is_rlang_prefixed(SEXP x, int (*sym_predicate)(SEXP)) {
  if (TYPEOF(x) != LANGSXP)
    return 0;

  if (!is_lang(CAR(x), "::"))
    return 0;

  SEXP args = CDAR(x);
  SEXP ns_sym = CAR(args);
  if (!is_sym(ns_sym, "rlang"))
    return 0;

  if (sym_predicate) {
    SEXP sym = CADR(args);
    return sym_predicate(sym);
  }

  return 1;
}
int is_rlang_call(SEXP x, int (*sym_predicate)(SEXP)) {
  if (TYPEOF(x) != LANGSXP)
    return false;
  else
    return sym_predicate(CAR(x)) || is_rlang_prefixed(x, sym_predicate);
}

SEXP last_cons(SEXP x) {
  while(CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}

SEXP rlang_length(SEXP x) {
  return Rf_ScalarInteger(Rf_length(x));
}

int is_true(SEXP x) {
  if (TYPEOF(x) != LGLSXP || Rf_length(x) != 1)
    Rf_errorcall(R_NilValue, "`x` must be a boolean");

  int value = LOGICAL(x)[0];
  return value == NA_LOGICAL ? 0 : value;
}

// Formulas --------------------------------------------------------------------

SEXP make_formula1(SEXP rhs, SEXP env) {
  SEXP f = PROTECT(Rf_lang2(Rf_install("~"), rhs));
  Rf_setAttrib(f, R_ClassSymbol, Rf_mkString("formula"));
  Rf_setAttrib(f, Rf_install(".Environment"), env);

  UNPROTECT(1);
  return f;
}

SEXP rlang_fun(SEXP sym) {
  SEXP prefixed_sym = PROTECT(Rf_lang3(Rf_install("::"), Rf_install("rlang"), sym));
  SEXP fun = Rf_eval(prefixed_sym, R_BaseEnv);
  UNPROTECT(1);
  return fun;
}

const char* kind_c_str(SEXPTYPE kind) {
  SEXP str = Rf_type2str(kind);
  return CHAR(str);
}

bool is_empty(SEXP x) {
  return Rf_length(x) == 0;
}

bool as_bool(SEXP x) {
  if (TYPEOF(x) != LGLSXP && Rf_length(x) != 1)
    Rf_errorcall(R_NilValue, "Expected a scalar logical");
   int* xp = (int*) LOGICAL(x);
   return *xp;
}

SEXP rlang_new_dictionary(SEXP x, SEXP lookup_msg, SEXP read_only) {
  SEXP dict = PROTECT(Rf_allocVector(VECSXP, 3));

  SET_VECTOR_ELT(dict, 0, x);
  SET_VECTOR_ELT(dict, 2, read_only);

  if (lookup_msg == R_NilValue)
    SET_VECTOR_ELT(dict, 1, Rf_mkString("Object `%s` not found in data"));
  else
    SET_VECTOR_ELT(dict, 1, lookup_msg);

  static SEXP nms = NULL;
  if (!nms) {
    nms = Rf_allocVector(STRSXP, 3);
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

  UNPROTECT(1);
  return dict;
}
