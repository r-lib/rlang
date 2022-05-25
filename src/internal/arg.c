#include <rlang.h>
#include "internal.h"
#include "nse-inject.h"
#include "utils.h"

#include "decl/arg-decl.h"


// Capture ----------------------------------------------------------------

static
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

r_obj* ffi_enexpr(r_obj* sym, r_obj* frame) {
  return capture(sym, frame, NULL);
}
r_obj* ffi_ensym(r_obj* sym, r_obj* frame) {
  r_obj* expr = capture(sym, frame, NULL);

  if (is_quosure(expr)) {
    expr = quo_get_expr(expr);
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
    // FIXME: Should call `abort_coercion()`
    r_abort("Can't convert to a symbol.");
  }

  return expr;
}


r_obj* ffi_enquo(r_obj* sym, r_obj* frame) {
  r_obj* env;
  r_obj* expr = KEEP(capture(sym, frame, &env));
  r_obj* quo = forward_quosure(expr, env);
  FREE(1);
  return quo;
}


// Match ------------------------------------------------------------------

static
int arg_match(r_obj* arg,
              r_obj* values,
              struct r_lazy error_arg,
              struct r_lazy error_call,
              struct r_lazy call) {
  if (r_typeof(values) != R_TYPE_character) {
    r_abort_lazy_call(call, "`values` must be a character vector.");
  }

  int values_len = r_length(values);
  if (values_len == 0) {
    r_abort_lazy_call(call, "`values` must have at least one element.");
  }

  switch (r_typeof(arg)) {
  case R_TYPE_character:
    break;
  case R_TYPE_string:
    return arg_match1(arg, values, error_arg, error_call);
  case R_TYPE_symbol:
    return arg_match1(r_sym_string(arg), values, error_arg, error_call);
  default:
    r_abort_lazy_call(error_call,
                      "%s must be a string or character vector.",
                      r_format_lazy_error_arg(error_arg));
  }

  int arg_len = r_length(arg);

  if (arg_len == 1) {
    return arg_match1(r_chr_get(arg, 0), values, error_arg, error_call);
  }

  if (arg_len != values_len) {
    r_abort_lazy_call(call, "`arg` must be a string or have the same length as `values`.");
  }

  r_obj* const* v_values = r_chr_cbegin(values);
  r_obj* const* v_arg = r_chr_cbegin(arg);

  // Same-length vector: must be identical, we allow changed order.
  int i = 0;
  for (; i < arg_len; ++i) {
    if (v_arg[i] != v_values[i]) {
      break;
    }
  }

  // Elements are identical, return first
  if (i == arg_len) {
    return 0;
  }

  r_obj* my_values = KEEP(r_clone(values));
  r_obj* const * v_my_values = r_chr_cbegin(my_values);

  // Invariant: my_values[i:(len-1)] contains the values we haven't matched yet
  for (; i < arg_len; ++i) {
    r_obj* current_arg = v_arg[i];
    if (current_arg == v_my_values[i]) {
      continue;
    }

    bool matched = false;
    for (int j = i + 1; j < arg_len; ++j) {
      if (current_arg == v_my_values[j]) {
        matched = true;

        // Replace matched value by the element that failed to match at this iteration
        r_chr_poke(my_values, j, v_my_values[i]);
        break;
      }
    }

    if (!matched) {
      r_eval_with_wxyz(stop_arg_match_call,
                       arg,
                       values,
                       KEEP(lazy_wrap_chr(error_arg)),
                       KEEP(r_lazy_eval(error_call)),
                       rlang_ns_env);
      r_stop_unreachable();
    }
  }

  r_obj* first_elt = r_chr_get(arg, 0);
  for (i = 0; i < values_len; ++i) {
    if (first_elt == v_values[i]) {
      FREE(1);
      return i;
    }
  }

  r_stop_unreachable();
}

int arg_match_legacy(r_obj* arg,
                     r_obj* values,
                     r_obj* error_arg,
                     r_obj* error_call) {
  struct r_lazy lazy_error_arg = { error_arg, r_null };
  struct r_lazy lazy_error_call = { error_call, r_null };

  return arg_match(arg,
                   values,
                   lazy_error_arg,
                   lazy_error_call,
                   r_lazy_null);
}

static
int arg_match1(r_obj* arg,
               r_obj* values,
               struct r_lazy error_arg,
               struct r_lazy error_call) {
  // Simple case: one argument, we check if it's one of the values
  r_obj* const* v_values = r_chr_cbegin(values);
  int n_values = r_length(values);

  for (int i = 0; i < n_values; ++i) {
    if (arg == v_values[i]) {
      return i;
    }
  }

  r_obj* ffi_error_call = r_lazy_eval(error_call);
  if (ffi_error_call == r_missing_arg) {
    // Replace `error_call` by environment on the stack because
    // `r_eval_with_` evaluates in an out-of-stack mask
    ffi_error_call = r_peek_frame();
  }
  KEEP(ffi_error_call);

  r_eval_with_wxyz(stop_arg_match_call,
                   KEEP(wrap_chr(arg)),
                   values,
                   KEEP(lazy_wrap_chr(error_arg)),
                   ffi_error_call,
                   rlang_ns_env);
  r_stop_unreachable();
}

static
r_obj* wrap_chr(r_obj* arg) {
  switch (arg_match_arg_nm_type(arg)) {
  case R_TYPE_string:
    return r_str_as_character(arg);
  case R_TYPE_symbol:
    return r_sym_as_utf8_character(arg);
  case R_TYPE_character:
    return arg;
  default:
    r_stop_unreachable();
  }
}

static
r_obj* lazy_wrap_chr(struct r_lazy arg) {
  r_obj* ffi_arg = KEEP(r_lazy_eval(arg));
  r_obj* out = wrap_chr(ffi_arg);
  FREE(1);
  return out;
}

static
enum r_type arg_match_arg_nm_type(r_obj* arg_nm) {
  switch (r_typeof(arg_nm)) {
  case R_TYPE_string: return R_TYPE_string;
  case R_TYPE_symbol: return R_TYPE_symbol;
  case R_TYPE_character:
    if (r_is_string(arg_nm)) {
      return R_TYPE_character;
    }
    // else fallthrough;
  default:
      r_abort("`arg_nm` must be a string or symbol.");
  }
}


int cci_arg_match(r_obj* arg,
                  r_obj* values,
                  struct r_lazy error_arg,
                  struct r_lazy error_call) {
  return arg_match(arg, values, error_arg, error_call, r_lazy_null);
}

r_obj* ffi_arg_match0(r_obj* args) {
  args = r_node_cdr(args);

  r_obj* arg = r_node_car(args); args = r_node_cdr(args);
  r_obj* values = r_node_car(args); args = r_node_cdr(args);
  r_obj* frame = r_node_car(args);

  struct r_lazy error_arg = { .x = syms.arg_nm, .env = frame };
  struct r_lazy error_call = { .x = r_syms.error_call, .env = frame };
  struct r_lazy call = { .x = frame, .env = r_null };

  int i = arg_match(arg, values, error_arg, error_call, call);
  return r_str_as_character(r_chr_get(values, i));
}


void rlang_init_arg(r_obj* ns) {
  stop_arg_match_call = r_parse("stop_arg_match(w, x, y, z)");
  r_preserve(stop_arg_match_call);
}

static r_obj* stop_arg_match_call = NULL;
