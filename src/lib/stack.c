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


static sexp* current_frame_call = NULL;

sexp* r_current_frame() {
  return r_eval(current_frame_call, r_empty_env);
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


void r_init_library_stack() {

  sexp* current_frame_args;
  current_frame_args = KEEP(r_scalar_int(-1));
  current_frame_args = KEEP(r_new_node(current_frame_args, r_null));
  sexp* current_frame_body = r_new_call_node(r_base_ns_get("sys.frame"),
                                             current_frame_args);

  sexp* current_frame_fn = KEEP(r_new_function(r_null,
                                               current_frame_body,
                                               r_empty_env));

  current_frame_call = r_new_call_node(current_frame_fn, r_null);
  r_mark_precious(current_frame_call);
  FREE(3);
}
