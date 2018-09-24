#include "rlang.h"


bool r_env_binding_is_promise(sexp* env, sexp* sym) {
  if (r_typeof(sym) != r_type_symbol) {
    r_abort("Internal error: Expected symbol in promise binding predicate");
  }

  sexp* obj = r_env_find(env, sym);
  return r_typeof(obj) == r_type_promise && PRVALUE(obj) == r_unbound_sym;
}
bool r_env_binding_is_active(sexp* env, sexp* sym) {
  if (r_typeof(sym) != r_type_symbol) {
    r_abort("Internal error: Expected symbol in active binding predicate");
  }
  return R_BindingIsActive(sym, env);
}
