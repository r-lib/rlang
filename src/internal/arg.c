#include <rlang.h>
#include "expr-interp.h"

// Capture

sexp* rlang_ns_get(const char* name);

sexp* capture(sexp* sym, sexp* frame, SEXP* arg_env) {
  static sexp* capture_call = NULL;
  if (!capture_call) {
    sexp* args = KEEP(r_new_node(r_null, r_null));
    capture_call = r_new_call(rlang_ns_get("captureArgInfo"), args);
    r_mark_precious(capture_call);
    r_mark_shared(capture_call);
    FREE(1);
  }

  if (r_typeof(sym) != SYMSXP) {
    r_abort("`arg` must be a symbol");
  }

  r_node_poke_cadr(capture_call, sym);
  sexp* arg_info = KEEP(r_eval(capture_call, frame));
  sexp* expr = r_list_get(arg_info, 0);
  sexp* env = r_list_get(arg_info, 1);

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

sexp* rlang_enexpr(sexp* sym, sexp* frame) {
  return capture(sym, frame, NULL);
}
sexp* rlang_ensym(sexp* sym, sexp* frame) {
  sexp* expr = capture(sym, frame, NULL);

  if (rlang_is_quosure(expr)) {
    expr = rlang_quo_get_expr(expr);
  }

  switch (r_typeof(expr)) {
  case r_type_symbol:
    break;
  case r_type_character:
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


sexp* rlang_enquo(sexp* sym, sexp* frame) {
  sexp* env;
  sexp* expr = KEEP(capture(sym, frame, &env));
  sexp* quo = forward_quosure(expr, env);
  FREE(1);
  return quo;
}

sexp* rlang_ext2_is_missing(sexp* _call, sexp* _op, sexp* args, sexp* env) {
  args = r_node_cdr(args);

  sexp* missing = r_eval(r_node_car(args), env);
  if (r_lgl_get(missing, 0)) {
    return r_shared_true;
  }

  return r_lgl(r_eval(r_x_sym, env) == r_missing_sym);
}

static sexp* stop_arg_match_call = NULL;
void arg_match0_abort(const char* msg, sexp* arg_nm_promise, sexp* env);
sexp* rlang_ext2_arg_match0(sexp* _call, sexp* _op, sexp* args, sexp* env) {
  args = r_node_cdr(args);

  sexp* arg =            KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  sexp* values =         KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  // Expensive by default, forced only if necessary.
  sexp* arg_nm_promise = r_node_car(args);

  if (r_typeof(arg) != r_type_character) {
    arg_match0_abort("`%s` must be a character vector.", arg_nm_promise, env);
  }
  if (r_typeof(values) != r_type_character) {
    r_abort("`values` must be a character vector.");
  }

  r_ssize len = r_length(arg);
  r_ssize len_values = r_length(values);
  if (len != 1 && len != len_values) {
    //arg_match0_abort("`%s` must be a string or have the same length as `values`.",  arg_nm_promise, env);

    arg = Rf_ScalarString(STRING_ELT(arg, 0));
    r_eval_with_xyz(stop_arg_match_call, r_base_env, arg, values, arg_nm_promise);

    while (1); // No return
  }

  if (len == 1) {
    sexp* arg_char = STRING_ELT(arg, 0);
    for (r_ssize i = 0; i < len_values; ++i) {
      if (arg_char == STRING_ELT(values, i)) {
        FREE(2);
        return(Rf_ScalarString(arg_char));
      }
    }

    r_eval_with_xyz(stop_arg_match_call, r_base_env, arg, values, arg_nm_promise);

    while (1); // No return
  } else {
    bool need_match = false;
    r_ssize ii = 0;
    for (; ii < len; ++ii) {
      if (STRING_ELT(arg, ii) != STRING_ELT(values, ii)) {
        need_match = true;
        break;
      }
    }

    // Elements are in order, return first
    if (!need_match) {
      FREE(2);
      return(Rf_ScalarString(STRING_ELT(values, 0)));
    }

    for (R_xlen_t i = ii; i < len; ++i) {
      bool matched = false;
      for (r_ssize j = ii; j < len; ++j) {
        if (STRING_ELT(arg, i) == STRING_ELT(values, j)) {
          matched = true;
          break;
        }
      }

      if (!matched) {
        // arg_match0_abort("`%s` must contain all elements in `values`.", arg_nm_promise, env);
        arg = Rf_ScalarString(STRING_ELT(arg, 0));
        r_eval_with_xyz(stop_arg_match_call, r_base_env, arg, values, arg_nm_promise);

        while (1); // No return
      }
    }

    FREE(2);
    return(Rf_ScalarString(STRING_ELT(arg, 0)));
  }
}

void arg_match0_abort(const char* msg, sexp* arg_nm_promise, sexp* env) {
  sexp* arg_nm = KEEP(r_eval(arg_nm_promise, env));

  if (r_typeof(arg_nm) != r_type_character || r_length(arg_nm) != 1) {
    r_abort("`arg_nm` must be a string.");
  }

  const char* arg_nm_chr = CHAR(STRING_ELT(arg_nm, 0));
  r_abort(msg, arg_nm_chr);
}

void r_init_library_arg() {
  stop_arg_match_call = r_parse("rlang:::stop_arg_match(x, y, z)");
  r_mark_precious(stop_arg_match_call);
}
