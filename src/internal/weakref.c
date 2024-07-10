#include <rlang.h>

r_obj* ffi_new_weakref(r_obj* key, r_obj* value, r_obj* finalizer, r_obj* on_quit) {
  if (r_typeof(key) != ENVSXP && r_typeof(key) != EXTPTRSXP) {
    r_abort("`key` must be an environment or external pointer");
  }
  return R_MakeWeakRef(key, value, finalizer, r_lgl_begin(on_quit)[0]);
}

r_obj* ffi_wref_key(r_obj* x) {
  if (r_typeof(x) != WEAKREFSXP) {
    r_abort("`x` must be a weak reference object");
  }
  return R_WeakRefKey(x);
}

r_obj* ffi_wref_value(r_obj* x) {
  if (r_typeof(x) != WEAKREFSXP) {
    r_abort("`x` must be a weak reference object");
  }
  return R_WeakRefValue(x);
}

r_obj* ffi_is_weakref(r_obj* x) {
  return Rf_ScalarLogical(r_typeof(x) == WEAKREFSXP);
}
