#include <rlang.h>
#include "internal.h"

// From call.c
sexp* rlang_call2(sexp* fn, sexp* args, sexp* ns);


sexp* rlang_exec(sexp* call, sexp* op, sexp* args, sexp* rho) {
  args = r_node_cdr(args);

  sexp* fn = KEEP(r_eval(r_sym(".fn"), rho));
  sexp* env = KEEP(r_eval(r_sym(".env"), rho));
  sexp* dots = KEEP(rlang_dots(rho));

  sexp* exec_call = KEEP(rlang_call2(fn, dots, r_null));

  sexp* node = r_node_cdr(exec_call);
  while (node != r_null) {
    sexp* arg = r_node_car(node);

    // Protect all symbolic arguments from being evaluated
    if (r_is_symbolic(arg)) {
      r_node_poke_car(node, r_call2(fns_quote, arg));
    }

    node = r_node_cdr(node);
  }

  sexp* out = r_eval(exec_call, env);

  FREE(4);
  return out;
}
