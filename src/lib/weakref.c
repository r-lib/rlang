#include <R.h>
#include <Rdefines.h>

sexp* rlang_new_weakref(sexp* key, sexp* value, sexp* finalizer, sexp* on_quit) {
  if (r_typeof(key) != ENVSXP && r_typeof(key) != EXTPTRSXP) {
    r_abort("`key` must be an environment or external pointer");
  }
  return R_MakeWeakRef(key, value, finalizer, LOGICAL(on_quit)[0]);
}

sexp* rlang_wref_key(sexp* x) {
  if (r_typeof(x) != WEAKREFSXP) {
    r_abort("`x` must be a weak reference object");
  }
  return R_WeakRefKey(x);
}

sexp* rlang_wref_value(sexp* x) {
  if (r_typeof(x) != WEAKREFSXP) {
    r_abort("`x` must be a weak reference object");
  }
  return R_WeakRefValue(x);
}

sexp* rlang_is_weakref(sexp* x) {
  return Rf_ScalarLogical(r_typeof(x) == WEAKREFSXP);
}
