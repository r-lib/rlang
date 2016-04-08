#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>

bool is_scalar(SEXP x) {
  return Rf_isVectorAtomic(x) && Rf_length(x) == 1;
}

bool is_call_to(SEXP x, const char* f) {
  if (!Rf_isLanguage(x))
    return false;

  SEXP fun = CAR(x);
  if (!Rf_isSymbol(fun))
    return false;

  return fun == Rf_install(f);
}

bool is_lazy_load(SEXP x) {
  if (TYPEOF(x) != PROMSXP)
    return false;

  return is_call_to(PREXPR(x), "lazyLoadDBfetch");
}

SEXP rhs(SEXP f) {
  if (TYPEOF(f) != LANGSXP || !Rf_inherits(f, "formula"))
    Rf_errorcall(R_NilValue, "`x` is not a formula");

  switch (Rf_length(f)) {
  case 2: return CADR(f);
  case 3: return CADDR(f);
  default: Rf_errorcall(R_NilValue, "Invalid formula");
  }
}

SEXP findLast(SEXP x) {
  SEXP cons = x;
  while(CDR(cons) != R_NilValue)
    cons = CDR(cons);

  return cons;
}
