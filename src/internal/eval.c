#include <rlang.h>
#include "internal.h"

// From call.c
r_obj* rlang_call2(r_obj* fn, r_obj* args, r_obj* ns);


r_obj* ffi_exec(r_obj* call, r_obj* op, r_obj* args, r_obj* rho) {
  args = r_node_cdr(args);

  r_obj* fn = KEEP(r_eval(r_sym(".fn"), rho));
  r_obj* env = KEEP(r_eval(r_sym(".env"), rho));
  r_obj* dots = KEEP(rlang_dots(rho));

  r_obj* exec_call = KEEP(rlang_call2(fn, dots, r_null));

  r_obj* node = r_node_cdr(exec_call);
  while (node != r_null) {
    r_obj* arg = r_node_car(node);

    // Protect all symbolic arguments from being evaluated
    if (r_is_symbolic(arg)) {
      r_node_poke_car(node, r_call2(fns_quote, arg));
    }

    node = r_node_cdr(node);
  }

  r_obj* out = r_eval(exec_call, env);

  FREE(4);
  return out;
}
