#include <rlang.h>
#include "nse-inject.h"
#include "utils.h"

#include "decl/arg-decl.h"


// Capture ----------------------------------------------------------------

r_obj* capture(r_obj* sym, r_obj* frame, r_obj** arg_env) {
  static r_obj* capture_call = NULL;
  if (!capture_call) {
    r_obj* args = KEEP(r_new_node(r_null, r_null));
    capture_call = r_new_call(rlang_ns_get("captureArgInfo"), args);
    r_preserve(capture_call);
    r_mark_shared(capture_call);
    FREE(1);
  }

  if (r_typeof(sym) != SYMSXP) {
    r_abort("`arg` must be a symbol");
  }

  r_node_poke_cadr(capture_call, sym);
  r_obj* arg_info = KEEP(r_eval(capture_call, frame));
  r_obj* expr = r_list_get(arg_info, 0);
  r_obj* env = r_list_get(arg_info, 1);

  // Unquoting rearranges the expression
  // FIXME: Only duplicate the call tree, not the leaves
  expr = KEEP(r_copy(expr));
  expr = call_interp(expr, env);

  if (arg_env) {
    *arg_env = env;
  }

  FREE(2);
  return expr;
}

r_obj* rlang_enexpr(r_obj* sym, r_obj* frame) {
  return capture(sym, frame, NULL);
}
r_obj* rlang_ensym(r_obj* sym, r_obj* frame) {
  r_obj* expr = capture(sym, frame, NULL);

  if (rlang_is_quosure(expr)) {
    expr = rlang_quo_get_expr(expr);
  }

  switch (r_typeof(expr)) {
  case R_TYPE_symbol:
    break;
  case R_TYPE_character:
    if (r_length(expr) == 1) {
      KEEP(expr);
      expr = r_sym(r_chr_get_c_string(expr, 0));
      FREE(1);
      break;
    }
    // else fallthrough
  default:
    r_abort("Only strings can be converted to symbols");
  }

  return expr;
}


r_obj* rlang_enquo(r_obj* sym, r_obj* frame) {
  r_obj* env;
  r_obj* expr = KEEP(capture(sym, frame, &env));
  r_obj* quo = forward_quosure(expr, env);
  FREE(1);
  return quo;
}


// Match ------------------------------------------------------------------

r_obj* ffi_arg_match0(r_obj* args) {
  args = r_node_cdr(args);

  r_obj* arg = r_node_car(args); args = r_node_cdr(args);
  r_obj* values = r_node_car(args); args = r_node_cdr(args);
  r_obj* env = r_node_car(args);

  if (r_typeof(arg) != R_TYPE_character) {
    arg_match0_abort("`%s` must be a character vector.", env);
  }
  if (r_typeof(values) != R_TYPE_character) {
    r_abort("`values` must be a character vector.");
  }

  r_ssize arg_len = r_length(arg);
  r_ssize values_len = r_length(values);
  if (values_len == 0) {
    arg_match0_abort("`values` must have at least one element.", env);
  }
  if (arg_len != 1 && arg_len != values_len) {
    arg_match0_abort("`%s` must be a string or have the same length as `values`.", env);
  }

  // Simple case: one argument, we check if it's one of the values.
  if (arg_len == 1) {
    r_obj* arg_char = r_chr_get(arg, 0);
    for (r_ssize i = 0; i < values_len; ++i) {
      if (arg_char == r_chr_get(values, i)) {
        return(arg);
      }
    }

    r_obj* arg_nm = KEEP(r_eval(arg_nm_sym, env));
    r_eval_with_xyz(stop_arg_match_call, arg, values, arg_nm, rlang_ns_env);

    r_stop_unreached("rlang_ext2_arg_match0");
  }

  r_obj* const* p_arg = r_chr_cbegin(arg);
  r_obj* const* p_values = r_chr_cbegin(values);

  // Same-length vector: must be identical, we allow changed order.
  r_ssize i = 0;
  for (; i < arg_len; ++i) {
    if (p_arg[i] != p_values[i]) {
      break;
    }
  }

  // Elements are identical, return first
  if (i == arg_len) {
    return(r_str_as_character(p_arg[0]));
  }

  r_obj* my_values = KEEP(r_clone(values));
  r_obj* const * p_my_values = r_chr_cbegin(my_values);

  // Invariant: my_values[i:(len-1)] contains the values we haven't matched yet
  for (; i < arg_len; ++i) {
    r_obj* current_arg = p_arg[i];
    if (current_arg == p_my_values[i]) {
      continue;
    }

    bool matched = false;
    for (r_ssize j = i + 1; j < arg_len; ++j) {
      if (current_arg == p_my_values[j]) {
        matched = true;

        // Replace matched value by the element that failed to match at this iteration
        r_chr_poke(my_values, j, p_my_values[i]);
        break;
      }
    }

    if (!matched) {
      arg = KEEP(r_str_as_character(r_chr_get(arg, 0)));
      r_obj* arg_nm = KEEP(r_eval(arg_nm_sym, env));
      r_eval_with_xyz(stop_arg_match_call, arg, values, arg_nm, rlang_ns_env);

      r_stop_unreached("rlang_ext2_arg_match0");
    }
  }

  FREE(1);
  return(r_str_as_character(r_chr_get(arg, 0)));
}

void arg_match0_abort(const char* msg, r_obj* env) {
  r_obj* arg_nm = KEEP(r_eval(arg_nm_sym, env));

  if (r_typeof(arg_nm) != R_TYPE_character || r_length(arg_nm) != 1) {
    r_abort(msg, "<arg_nm>");
  }

  const char* arg_nm_chr = r_chr_get_c_string(arg_nm, 0);
  r_abort(msg, arg_nm_chr);
}


void rlang_init_arg(r_obj* ns) {
  stop_arg_match_call = r_parse("stop_arg_match(x, y, z)");
  r_preserve(stop_arg_match_call);

  arg_nm_sym = r_sym("arg_nm");
}

static r_obj* stop_arg_match_call = NULL;
static r_obj* arg_nm_sym = NULL;
