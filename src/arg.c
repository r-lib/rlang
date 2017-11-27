#include "rlang/rlang.h"
#include "expr-interp.h"

// Capture

SEXP rlang_ns_get(const char* name);

SEXP capture(SEXP sym, SEXP frame, SEXP unquote_names, SEXP* arg_env) {
  static SEXP capture_call = NULL;
  if (!capture_call) {
    SEXP args = KEEP(r_new_node(r_null, r_null));
    capture_call = r_new_call_node(rlang_ns_get("captureArgInfo"), args);
    r_mark_precious(capture_call);
    r_mark_shared(capture_call);
    FREE(1);
  }

  if (r_kind(sym) != SYMSXP) {
    r_abort("`arg` must be a symbol");
  }

  r_node_poke_cadr(capture_call, sym);
  SEXP arg_info = KEEP(r_eval(capture_call, frame));
  SEXP expr = r_list_get(arg_info, 0);
  SEXP env = r_list_get(arg_info, 1);

  // Unquoting rearranges the expression
  expr = KEEP(r_duplicate(expr, false));
  expr = call_interp(expr, env, r_as_bool(unquote_names));

  if (arg_env) {
    *arg_env = env;
  }

  FREE(2);
  return expr;
}

SEXP rlang_enexpr(SEXP sym, SEXP frame, SEXP unquote_names) {
  return capture(sym, frame, unquote_names, NULL);
}


SEXP forward_quosure(SEXP x, SEXP env);

SEXP rlang_enquo(SEXP sym, SEXP frame, SEXP unquote_names) {
  SEXP env;
  SEXP expr = KEEP(capture(sym, frame, unquote_names, &env));
  SEXP quo = forward_quosure(expr, env);
  FREE(1);
  return quo;
}
