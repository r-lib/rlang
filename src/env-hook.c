#include "rlang.h"

#include <R_ext/Callbacks.h>
#include <stdlib.h>

void signal_lookup(const char* name, R_ObjectTable* env) {
  // Avoid infloop while signalling
  if (env->active)
    env->active = FALSE;
  else
    return ;

  static SEXP type = NULL;
  if (!type) {
    type = Rf_allocVector(STRSXP, 2);
    R_PreserveObject(type);
    mut_chr_at(type, 0, r_string("eval_lookup"));
    mut_chr_at(type, 1, r_string("eval"));
  }

  SEXP data = PROTECT(Rf_allocVector(VECSXP, 1));
  mut_list_at(data, 0, string(name));
  mut_names(data, string("symbol"));

  SEXP cnd = PROTECT(new_condition(type, data, R_NilValue));
  cnd_signal(cnd, true);

  env->active = TRUE;
  UNPROTECT(2);
}

Rboolean hook_env_exists(const char* const name, Rboolean* can_cache, R_ObjectTable* env) {
  signal_lookup(name, env);
  return FALSE;
}
SEXP hook_env_get(const char* const name, Rboolean* can_cache, R_ObjectTable* env) {
  signal_lookup(name, env);
  return R_UnboundValue;
}
int hook_env_remove(const char* const name, R_ObjectTable* env) {
  signal_lookup(name, env);
  return 0;
}
SEXP hook_env_assign(const char* const name, SEXP value, R_ObjectTable* env) {
  r_abort("Internal error: Assignment in the hook environment");
  return R_NilValue;
}
SEXP hook_env_objects(R_ObjectTable* env) {
  return Rf_allocVector(STRSXP, 0);
}
Rboolean hook_env_can_cache(const char* const name, R_ObjectTable* env) {
  return TRUE;
}
void hook_env_on_attach(R_ObjectTable* env) {
  r_abort("Internal error: Hook environment was attached");
}
void hook_env_on_detach(R_ObjectTable* env) {
  r_abort("Internal error: Hook environment was detached");
}

// Returns an environment without frame or hashtab
SEXP bare_environment() {
  SEXP env = Rf_cons(R_NilValue, R_EmptyEnv);
  SET_TYPEOF(env, ENVSXP);
  return env;
}

SEXP init_hook_env() {
  R_ObjectTable* env_struct = (R_ObjectTable*) calloc(1, sizeof(R_ObjectTable));
  env_struct->exists = &hook_env_exists;
  env_struct->get = &hook_env_get;
  env_struct->remove = &hook_env_remove;
  env_struct->assign = &hook_env_assign;
  env_struct->objects = &hook_env_objects;
  env_struct->canCache = &hook_env_can_cache;
  env_struct->onAttach = &hook_env_on_attach;
  env_struct->onDetach = &hook_env_on_detach;
  env_struct->active = TRUE;

  SEXP env_ptr = PROTECT(R_MakeExternalPtr(env_struct, R_NilValue, R_NilValue));
  mut_class(env_ptr, string("UserDefinedDatabase"));

  SEXP env = PROTECT(bare_environment());
  SET_HASHTAB(env, env_ptr);

  SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
  mut_chr_at(classes, 0, r_string("hook_env"));
  mut_chr_at(classes, 1, r_string("UserDefinedDatabase"));
  mut_class(env, classes);

  UNPROTECT(3);
  return env;
}

SEXP rlang_hook_env() {
  static SEXP hook_env = NULL;
  if (!hook_env) {
    hook_env = init_hook_env();
    R_PreserveObject(hook_env);
  }
  return hook_env;
}
