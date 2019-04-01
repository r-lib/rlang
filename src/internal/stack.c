#include <rlang.h>

// Initialised at load time
static sexp* fns_callback;


void rlang_on_exit_callback(void (*fn)(void*), void* data) {
  sexp* fn_extptr = KEEP(R_MakeExternalPtrFn((r_fn_ptr) fn, r_null, r_null));
  sexp* data_extptr = KEEP(R_MakeExternalPtr(data, r_null, r_null));
  sexp* call = KEEP(Rf_lang3(fns_callback, fn_extptr, data_extptr));

  sexp* frame = KEEP(r_current_frame());
  r_on_exit(call, frame);

  FREE(4);
}

void rlang_init_stack() {
  fns_callback = rlang_ns_get("callback");
}
