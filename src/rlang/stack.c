#include "rlang.h"

sexp* rlang_ns_get(const char* name);


void r_on_exit(sexp* expr, sexp* frame) {
  static sexp* on_exit_prim = NULL;
  if (!on_exit_prim) {
    on_exit_prim = r_base_ns_get("on.exit");
  }

  sexp* args = r_pairlist2(expr, r_lgl(1));
  sexp* lang = KEEP(r_new_call(on_exit_prim, args));

  r_eval(lang, frame);
  FREE(1);
}


static sexp* current_frame_call = NULL;

sexp* r_peek_frame() {
  return r_eval(current_frame_call, r_empty_env);
}


static sexp* sys_frame_call = NULL;
static sexp* sys_call_call = NULL;

static int* sys_frame_n_addr = NULL;
static int* sys_call_n_addr = NULL;

sexp* r_sys_frame(int n, sexp* frame) {
  int n_kept = 0;
  if (!frame) {
    frame = r_peek_frame();
    KEEP_N(frame, &n_kept);
  }

  *sys_frame_n_addr = n;
  SEXP value = r_eval(sys_frame_call, frame);

  FREE(n_kept);
  return value;
}
sexp* r_sys_call(int n, sexp* frame) {
  int n_kept = 0;
  if (!frame) {
    frame = r_peek_frame();
    KEEP_N(frame, &n_kept);
  }

  *sys_call_n_addr = n;
  SEXP value = r_eval(sys_call_call, frame);

  FREE(n_kept);
  return value;
}


static sexp* generate_sys_call(const char* name, int** n_addr) {
  sexp* sys_n = KEEP(r_int(0));
  *n_addr = r_int_deref(sys_n);

  sexp* sys_args = KEEP(r_new_node(sys_n, r_null));
  sexp* sys_call = KEEP(r_new_call(r_base_ns_get(name), sys_args));
  r_preserve(sys_call);

  FREE(3);
  return sys_call;
}

void r_init_library_stack() {
  sexp* current_frame_body = KEEP(r_parse_eval("as.call(list(sys.frame, -1))", r_base_env));
  sexp* current_frame_fn = KEEP(r_new_function(r_null, current_frame_body, r_empty_env));
  current_frame_call = r_new_call(current_frame_fn, r_null);
  r_preserve(current_frame_call);
  FREE(2);

  sys_frame_call = generate_sys_call("sys.frame", &sys_frame_n_addr);
  sys_call_call = generate_sys_call("sys.call", &sys_call_n_addr);
}
