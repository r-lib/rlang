#include "rlang.h"
#include "export.h"
#include <Rversion.h>

r_obj* rlang_namespace(const char* ns) {
  r_obj* ns_string = KEEP(Rf_mkString(ns));
  r_obj* call = KEEP(r_sym("getNamespace"));
  call = KEEP(Rf_lang2(call, ns_string));
  r_obj* ns_env = r_eval(call, R_BaseEnv);
  FREE(3);
  return ns_env;
}
