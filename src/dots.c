#include "rlang/rlang.h"

SEXP rlang_ns_get(const char* name);


SEXP rlang_capture_dots(SEXP frame_env) {
  static SEXP capture_call = NULL;

  if (!capture_call) {
    capture_call = KEEP(r_new_call_node(rlang_ns_get("captureDots"), r_null));
    r_preserve(capture_call);
    r_mark_shared(capture_call);
    FREE(1);
  }

  return r_eval(capture_call, frame_env);
}
