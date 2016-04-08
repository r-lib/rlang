#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

int is_lazy_load(SEXP x) {
  if (TYPEOF(x) != PROMSXP)
    return 0;

  SEXP expr = PREXPR(x);
  if (TYPEOF(expr) != LANGSXP)
    return 0;

  SEXP funname = CAR(expr);
  if (TYPEOF(funname) != SYMSXP)
    return 0;

  const char* name = CHAR(PRINTNAME(funname));
  if (strcmp(name, "lazyLoadDBfetch") == 0)
    return 1;

  return 0;
}
