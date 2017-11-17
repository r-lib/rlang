#include "rlang/rlang.h"

SEXP rlang_ns_get(const char* name);


SEXP rlang_capture_dots(SEXP frame_env) {
  static SEXP capture_call = NULL;

  if (!capture_call) {
    capture_call = KEEP(r_new_call_node(rlang_ns_get("captureDots"), r_null));
    capture_call = KEEP(r_new_node(capture_call, r_null));
    capture_call = r_new_call_node(r_base_ns_get("as.pairlist"), capture_call);
    r_preserve(capture_call);
    FREE(2);
  }

  return r_eval(capture_call, frame_env);
}
