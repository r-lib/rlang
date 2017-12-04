#include "rlang.h"

sexp* rlang_ns_get(const char* name);


void r_on_exit(sexp* expr, sexp* frame) {
  static sexp* on_exit_prim = NULL;
  if (!on_exit_prim) {
    on_exit_prim = r_base_ns_get("on.exit");
  }

  sexp* args = r_build_pairlist2(expr, r_scalar_lgl(1));
  sexp* lang = KEEP(r_build_call_node(on_exit_prim, args));

  r_eval(lang, frame);
  FREE(1);
}

sexp* r_current_frame() {
  static sexp* call = NULL;
  if (!call) call = rlang_ns_get("rlang_current_frame");

  return r_eval(call, r_empty_env);
}

sexp* r_sys_frame(int n, sexp* frame) {
  if (!frame) {
    frame = r_current_frame();
  }

  static sexp* call = NULL;
  static int* n_addr = NULL;
  if (!call) {
    call = rlang_ns_get("rlang_sys_frame");
    n_addr = INTEGER(r_node_cadr(call));
  }

  *n_addr = n;
  return r_eval(call, frame);
}

sexp* r_sys_call(int n, sexp* frame) {
  if (!frame) {
    frame = r_current_frame();
  }

  static sexp* call = NULL;
  static int* n_addr = NULL;
  if (!call) {
    call = rlang_ns_get("rlang_sys_call");
    n_addr = INTEGER(r_node_cadr(call));
  }

  *n_addr = n;
  return r_eval(call, frame);
}
