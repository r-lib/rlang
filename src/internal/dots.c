#include <rlang.h>
#include "dots.h"
#include "expr-interp.h"
#include "internal.h"
#include "squash.h"
#include "utils.h"
#include "vec.h"

r_obj* rlang_ns_get(const char* name);
static bool should_auto_name(r_obj* named);

static r_obj* as_label_call = NULL;

static inline
r_obj* r_as_label(r_obj* x) {
  return r_eval_with_x(as_label_call, x, rlang_ns_env);
}

// Initialised at load time
static r_obj* empty_spliced_arg = NULL;
static r_obj* splice_box_attrib = NULL;
static r_obj* quosures_attrib = NULL;

r_obj* rlang_new_splice_box(r_obj* x) {
  r_obj* out = KEEP(r_alloc_list(1));
  r_list_poke(out, 0, x);
  r_poke_attrib(out, splice_box_attrib);
  r_mark_object(out);
  FREE(1);
  return out;
}
bool is_splice_box(r_obj* x) {
  return r_attrib(x) == splice_box_attrib;
}
r_obj* rlang_is_splice_box(r_obj* x) {
  return r_lgl(is_splice_box(x));
}
r_obj* rlang_unbox(r_obj* x) {
  if (r_length(x) != 1) {
    r_abort("Internal error: Expected a list of size 1 in `rlang_unbox()`.");
  }
  return r_list_get(x, 0);
}


enum dots_homonyms {
  DOTS_HOMONYMS_keep = 0,
  DOTS_HOMONYMS_first,
  DOTS_HOMONYMS_last,
  DOTS_HOMONYMS_error
};

struct dots_capture_info {
  enum dots_collect type;
  r_ssize count;
  r_obj* named;
  bool needs_expansion;
  int ignore_empty;
  bool preserve_empty;
  bool unquote_names;
  enum dots_homonyms homonyms;
  bool check_assign;
  r_obj* (*big_bang_coerce)(r_obj*);
  bool splice;
};

static int arg_match_ignore_empty(r_obj* ignore_empty);
static enum dots_homonyms arg_match_homonyms(r_obj* homonyms);

struct dots_capture_info init_capture_info(enum dots_collect type,
                                           r_obj* named,
                                           r_obj* ignore_empty,
                                           r_obj* preserve_empty,
                                           r_obj* unquote_names,
                                           r_obj* homonyms,
                                           r_obj* check_assign,
                                           r_obj* (*coercer)(r_obj*),
                                           bool splice) {
  struct dots_capture_info info;

  info.type = type;
  info.count = 0;
  info.needs_expansion = false;
  info.named = named;
  info.ignore_empty = arg_match_ignore_empty(ignore_empty);
  info.preserve_empty = r_lgl_get(preserve_empty, 0);
  info.unquote_names = r_lgl_get(unquote_names, 0);
  info.homonyms = arg_match_homonyms(homonyms);
  info.check_assign = r_lgl_get(check_assign, 0);
  info.big_bang_coerce = coercer;
  info.splice = splice;

  return info;
}


static bool has_glue = false;
r_obj* rlang_glue_is_there() {
  has_glue = true;
  return r_null;
}

static bool has_curly(const char* str) {
  for (char c = *str; c != '\0'; ++str, c = *str) {
    if (c == '{') {
      return true;
    }
  }
  return false;
}

static void require_glue() {
  r_obj* call = KEEP(r_parse("is_installed('glue')"));
  r_obj* out = KEEP(r_eval(call, rlang_ns_env));

  if (!r_is_bool(out)) {
    r_abort("Internal error: Expected scalar logical from `requireNamespace()`.");
  }
  if (!r_lgl_get(out, 0)) {
    r_abort("Can't use `{` symbols in LHS of `:=` if glue is not installed.");
  }

  FREE(2);
}

// Initialised at load time
static r_obj* glue_unquote_fn = NULL;

static r_obj* glue_unquote(r_obj* lhs, r_obj* env) {
  if (r_typeof(lhs) != R_TYPE_character ||
      r_length(lhs) != 1 ||
      !has_curly(r_chr_get_c_string(lhs, 0))) {
    return lhs;
  }

  if (!has_glue) {
    require_glue();
  }

  r_obj* glue_unquote_call = KEEP(r_call2(glue_unquote_fn, lhs));
  lhs = r_eval(glue_unquote_call, env);
  FREE(1);
  return lhs;
}

static r_obj* def_unquote_name(r_obj* expr, r_obj* env) {
  int n_kept = 0;
  r_obj* lhs = r_node_cadr(expr);

  struct expansion_info info = which_expansion_op(lhs, true);

  switch (info.op) {
  case INJECTION_OP_none:
    lhs = KEEP_N(glue_unquote(lhs, env), &n_kept);
    break;
  case INJECTION_OP_uq:
    lhs = KEEP_N(r_eval(info.operand, env), &n_kept);
    break;
  case INJECTION_OP_curly:
    lhs = KEEP_N(rlang_enquo(info.operand, env), &n_kept);
    break;
  case INJECTION_OP_uqs:
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
  case INJECTION_OP_uqn:
    r_abort("Internal error: Chained `:=` should have been detected earlier");
  case INJECTION_OP_fixup:
    r_abort("The LHS of `:=` must be a string or a symbol");
  case INJECTION_OP_dot_data:
    r_abort("Can't use the `.data` pronoun on the LHS of `:=`");
  }

  // Unwrap quosures for convenience
  if (rlang_is_quosure(lhs)) {
    lhs = rlang_quo_get_expr_(lhs);
  }

  int err = 0;
  lhs = r_new_symbol(lhs, &err);
  if (err) {
    r_abort("The LHS of `:=` must be a string or a symbol");
  }

  FREE(n_kept);
  return lhs;
}

void signal_retired_splice() {
  const char* msg =
    "Unquoting language objects with `!!!` is deprecated as of rlang 0.4.0.\n"
    "Please use `!!` instead.\n"
    "\n"
    "  # Bad:\n"
    "  dplyr::select(data, !!!enquo(x))\n"
    "\n"
    "  # Good:\n"
    "  dplyr::select(data, !!enquo(x))    # Unquote single quosure\n"
    "  dplyr::select(data, !!!enquos(x))  # Splice list of quosures\n";
  warn_deprecated(msg, msg);
}

static r_obj* dots_big_bang_coerce(r_obj* x) {
  switch (r_typeof(x)) {
  case R_TYPE_null:
  case R_TYPE_pairlist:
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_character:
  case R_TYPE_raw:
    if (r_is_object(x)) {
      return r_eval_with_x(rlang_as_list_call, x, rlang_ns_env);
    } else {
      return r_vec_coerce(x, R_TYPE_list);
    }
  case R_TYPE_list:
    if (r_is_object(x)) {
      return r_eval_with_x(rlang_as_list_call, x, rlang_ns_env);
    } else {
      return x;
    }
  case R_TYPE_s4:
    return r_eval_with_x(rlang_as_list_call, x, rlang_ns_env);
  case R_TYPE_call:
    if (r_is_symbol(r_node_car(x), "{")) {
      return r_vec_coerce(r_node_cdr(x), R_TYPE_list);
    }
    // else fallthrough
  case R_TYPE_symbol:
    signal_retired_splice();
    return r_list(x);

  default:
    r_abort(
      "Can't splice an object of type `%s` because it is not a vector",
      r_type_as_c_string(r_typeof(x))
    );
  }
}

// Also used in expr-interp.c
r_obj* big_bang_coerce_pairlist(r_obj* x, bool deep) {
  int n_kept = 0;

  if (r_is_object(x)) {
    x = KEEP_N(dots_big_bang_coerce(x), &n_kept);
  }

  switch (r_typeof(x)) {
  case R_TYPE_null:
  case R_TYPE_pairlist:
    x = r_clone(x);
    break;
  case R_TYPE_logical:
  case R_TYPE_integer:
  case R_TYPE_double:
  case R_TYPE_complex:
  case R_TYPE_character:
  case R_TYPE_raw:
  case R_TYPE_list:
    // Check for length because `Rf_coerceVector()` to pairlist fails
    // with named empty vectors (#1045)
    if (r_length(x)) {
      x = r_vec_coerce(x, R_TYPE_pairlist);
    } else {
      x = r_null;
    }
    break;
  case R_TYPE_call:
    if (deep && r_is_symbol(r_node_car(x), "{")) {
      x = r_node_cdr(x);
      break;
    }
    // fallthrough
  case R_TYPE_symbol: {
    if (deep) {
      signal_retired_splice();
      x = r_new_node(x, r_null);
      break;
    }
    // fallthrough
  }
  default:
    r_abort(
      "Can't splice an object of type `%s` because it is not a vector",
      r_type_as_c_string(r_typeof(x))
    );
  }

  FREE(n_kept);
  return x;
}
static r_obj* dots_big_bang_coerce_pairlist(r_obj* x) {
  return big_bang_coerce_pairlist(x, false);
}

static r_obj* dots_big_bang_value(struct dots_capture_info* capture_info,
                                  r_obj* value, r_obj* env, bool quosured) {
  value = KEEP(capture_info->big_bang_coerce(value));

  r_ssize n = r_length(value);

  if (quosured) {
    if (r_is_shared(value)) {
      r_obj* tmp = r_clone(value);
      FREE(1);
      value = KEEP(tmp);
    }

    for (r_ssize i = 0; i < n; ++i) {
      r_obj* elt = r_list_get(value, i);
      elt = forward_quosure(elt, env);
      r_list_poke(value, i, elt);
    }
  }

  // The dots_values() variant does not splice for performance
  if (capture_info->splice) {
    capture_info->needs_expansion = true;
    capture_info->count += n;
  }

  value = rlang_new_splice_box(value);

  FREE(1);
  return value;
}
static r_obj* dots_big_bang(struct dots_capture_info* capture_info,
                            r_obj* expr, r_obj* env, bool quosured) {
  r_obj* value = KEEP(r_eval(expr, env));
  r_obj* out = dots_big_bang_value(capture_info, value, env, quosured);
  FREE(1);
  return out;
}

static inline bool should_ignore(int ignore_empty, r_ssize i, r_ssize n) {
  return ignore_empty == 1 || (i == n - 1 && ignore_empty == -1);
}
static inline r_obj* dot_get_expr(r_obj* dot) {
  return r_list_get(dot, 0);
}
static inline r_obj* dot_get_env(r_obj* dot) {
  return r_list_get(dot, 1);
}

static r_obj* dots_unquote(r_obj* dots, struct dots_capture_info* capture_info) {
  capture_info->count = 0;
  r_ssize n = r_length(dots);
  bool unquote_names = capture_info->unquote_names;

  // In the case of `dots_list()` we auto-name inputs eagerly while we
  // still have access to the defused expression
  bool needs_autoname =
    capture_info->type == DOTS_COLLECT_value &&
    should_auto_name(capture_info->named);

  r_obj* node = dots;
  for (r_ssize i = 0; node != r_null; ++i, node = r_node_cdr(node)) {
    r_obj* elt = r_node_car(node);
    r_obj* expr = dot_get_expr(elt);
    r_obj* env = dot_get_env(elt);

    // Unquoting rearranges expressions
    // FIXME: Only duplicate the call tree, not the leaves
    expr = KEEP(r_copy(expr));

    if (unquote_names && r_is_call(expr, ":=")) {
      if (r_node_tag(node) != r_null) {
        r_abort("Can't supply both `=` and `:=`");
      }

      r_obj* nm = def_unquote_name(expr, env);
      r_node_poke_tag(node, nm);
      expr = r_node_cadr(r_node_cdr(expr));
    }

    if (capture_info->check_assign
        && r_is_call(expr, "<-")
        && r_peek_option("rlang_dots_disable_assign_warning") == r_null) {
      r_warn(
        "Using `<-` as argument is often a mistake.\n"
        "Do you need to use `=` to match an argument?\n"
        "\n"
        "If you really want to use `<-`, please wrap in braces:\n"
        "\n"
        "  # Bad:\n"
        "  fn(a <- 1)\n"
        "\n"
        "  # Good:\n"
        "  fn(a = 1)       # Match 1 to parameter `a`\n"
        "  fn({ a <- 1 })  # Assign 1 to variable `a`"
      );
    }

    struct expansion_info info = which_expansion_op(expr, unquote_names);
    enum dots_op dots_op = info.op + (INJECTION_OP_MAX * capture_info->type);

    r_obj* name = r_node_tag(node);

    // Ignore empty arguments
    if (expr == r_syms.missing
        && (name == r_null || name == r_globals.empty_str)
        && should_ignore(capture_info->ignore_empty, i, n)) {
      capture_info->needs_expansion = true;
      r_node_poke_car(node, empty_spliced_arg);
      FREE(1);
      continue;
    }

    switch (dots_op) {
    case DOTS_OP_expr_none:
    case DOTS_OP_expr_uq:
    case DOTS_OP_expr_fixup:
    case DOTS_OP_expr_dot_data:
    case DOTS_OP_expr_curly:
      expr = call_interp_impl(expr, env, info);
      capture_info->count += 1;
      break;
    case DOTS_OP_expr_uqs:
      expr = dots_big_bang(capture_info, info.operand, env, false);
      break;
    case DOTS_OP_quo_none:
    case DOTS_OP_quo_uq:
    case DOTS_OP_quo_fixup:
    case DOTS_OP_quo_dot_data:
    case DOTS_OP_quo_curly: {
      expr = KEEP(call_interp_impl(expr, env, info));
      expr = forward_quosure(expr, env);
      FREE(1);
      capture_info->count += 1;
      break;
    }
    case DOTS_OP_quo_uqs: {
      expr = dots_big_bang(capture_info, info.operand, env, true);
      break;
    }
    case DOTS_OP_value_none:
    case DOTS_OP_value_fixup:
    case DOTS_OP_value_dot_data: {
      r_obj* orig = expr;

      if (expr == r_syms.missing) {
        if (!capture_info->preserve_empty) {
          r_abort("Argument %d is empty", i + 1);
        }
      } else if (env != r_empty_env) {
        // Don't evaluate when `env` is the empty environment. This
        // happens when the argument was forced (and thus already
        // evaluated), for instance by lapply() or map().
        expr = r_eval(expr, env);
      }

      r_keep_t i;
      KEEP_HERE(expr, &i);

      if (is_splice_box(expr)) {
        // Coerce contents of splice boxes to ensure uniform type
        expr = rlang_unbox(expr);
        expr = dots_big_bang_value(capture_info, expr, env, false);
        KEEP_AT(expr, i);
      } else {
        if (needs_autoname && r_node_tag(node) == r_null) {
          r_obj* label = KEEP(r_as_label(orig));
          r_node_poke_tag(node, r_str_as_symbol(r_chr_get(label, 0)));
          FREE(1);
        }
        capture_info->count += 1;
      }

      FREE(1);
      break;
    }
    case DOTS_OP_value_uq:
      r_abort("Can't use `!!` in a non-quoting function");
    case DOTS_OP_value_uqs: {
      expr = dots_big_bang(capture_info, info.operand, env, false);
      break;
    }
    case DOTS_OP_value_curly:
      r_abort("Can't use `{{` in a non-quoting function");
    case DOTS_OP_expr_uqn:
    case DOTS_OP_quo_uqn:
    case DOTS_OP_value_uqn:
      r_abort("`:=` can't be chained");
    case DOTS_OP_MAX:
      r_abort("Internal error: `DOTS_OP_MAX`");
    }

    r_node_poke_car(node, expr);
    FREE(1);
  }

  return dots;
}


static int arg_match_ignore_empty(r_obj* ignore_empty) {
  if (r_typeof(ignore_empty) != R_TYPE_character || r_length(ignore_empty) == 0) {
    r_abort("`.ignore_empty` must be a character vector");
  }
  const char* arg = r_chr_get_c_string(ignore_empty, 0);
  switch(arg[0]) {
  case 't': if (!strcmp(arg, "trailing")) return -1; else break;
  case 'n': if (!strcmp(arg, "none")) return 0; else break;
  case 'a': if (!strcmp(arg, "all")) return 1; else break;
  }
  r_abort("`.ignore_empty` must be one of: \"trailing\", \"none\", or \"all\"");
}

static enum dots_homonyms arg_match_homonyms(r_obj* homonyms) {
  if (r_typeof(homonyms) != R_TYPE_character || r_length(homonyms) == 0) {
    r_abort("`.homonyms` must be a character vector");
  }
  const char* arg = r_chr_get_c_string(homonyms, 0);
  switch(arg[0]) {
  case 'k': if (!strcmp(arg, "keep")) return DOTS_HOMONYMS_keep; else break;
  case 'f': if (!strcmp(arg, "first")) return DOTS_HOMONYMS_first; else break;
  case 'l': if (!strcmp(arg, "last")) return DOTS_HOMONYMS_last; else break;
  case 'e': if (!strcmp(arg, "error")) return DOTS_HOMONYMS_error; else break;
  }
  r_abort("`.homonyms` must be one of: \"keep\", \"first\", \"last\", or \"error\"");
}

static void warn_deprecated_width() {
  const char* msg = "`.named` can no longer be a width";
  warn_deprecated(msg, msg);
}
static bool should_auto_name(r_obj* named) {
  if (r_length(named) != 1) {
    goto error;
  }

  switch (r_typeof(named)) {
  case R_TYPE_logical:
    return r_lgl_get(named, 0);
  case R_TYPE_integer:
    warn_deprecated_width();
    return r_int_begin(named)[0];
  case R_TYPE_double:
    if (r_is_integerish(named, -1, true)) {
      warn_deprecated_width();
      return r_dbl_begin(named)[0];
    }
    // else fallthrough
  default:
    break;
  }

 error:
  r_abort("`.named` must be a scalar logical");
}

static r_obj* auto_name_call = NULL;

static r_obj* maybe_auto_name(r_obj* x, r_obj* named) {
  r_obj* names = r_names(x);

  if (should_auto_name(named) && (names == r_null || r_chr_has(names, ""))) {
    x = r_eval_with_x(auto_name_call, x, r_base_env);
  }

  return x;
}

static bool any_name(r_obj* x, bool splice) {
  while (x != r_null) {
    if (r_node_tag(x) != r_null) {
      return true;
    }

    r_obj* elt = r_node_car(x);

    if (splice && is_splice_box(elt)) {
      if (r_names(rlang_unbox(elt)) != r_null) {
        return true;
      }
    }

    x = r_node_cdr(x);
  }

  return false;
}

static void check_named_splice(r_obj* node) {
  if (r_node_tag(node) != r_null) {
    const char* msg = "`!!!` can't be supplied with a name. Only the operand's names are retained.";
    stop_defunct(msg);
  }
}

r_obj* dots_as_list(r_obj* dots, struct dots_capture_info* capture_info) {
  int n_kept = 0;

  r_obj* out = KEEP_N(r_alloc_list(capture_info->count), &n_kept);

  // Add default empty names unless dots are captured by values
  r_obj* out_names = r_null;
  if (capture_info->type != DOTS_COLLECT_value || any_name(dots, capture_info->splice)) {
    out_names = KEEP_N(r_alloc_character(capture_info->count), &n_kept);
    r_attrib_push(out, r_syms.names, out_names);
  }

  for (r_ssize i = 0, count = 0; dots != r_null; ++i, dots = r_node_cdr(dots)) {
    r_obj* elt = r_node_car(dots);

    if (elt == empty_spliced_arg) {
      continue;
    }

    if (capture_info->splice && is_splice_box(elt)) {
      check_named_splice(dots);

      elt = rlang_unbox(elt);
      r_obj* nms = r_names(elt);

      r_ssize n = r_length(elt);
      for (r_ssize i = 0; i < n; ++i) {
        r_obj* value = r_list_get(elt, i);
        r_list_poke(out, count, value);

        r_obj* name = r_nms_get(nms, i);
        if (name != r_globals.empty_str) {
          r_chr_poke(out_names, count, name);
        }

        ++count;
      }
    } else {
      r_list_poke(out, count, elt);

      r_obj* name = r_node_tag(dots);
      if (name != r_null) {
        r_chr_poke(out_names, count, r_str(r_sym_c_string(name)));
      }

      ++count;
    }
  }

  FREE(n_kept);
  return out;
}

r_obj* dots_as_pairlist(r_obj* dots, struct dots_capture_info* capture_info) {
  r_obj* out = KEEP(r_new_node(r_null, dots));
  r_obj* prev = out;

  while (dots != r_null) {
    r_obj* elt = r_node_car(dots);

    if (elt == empty_spliced_arg) {
      dots = r_node_cdr(dots);
      r_node_poke_cdr(prev, dots);
      continue;
    }

    if (capture_info->splice && is_splice_box(elt)) {
      check_named_splice(dots);

      elt = rlang_unbox(elt);
      if (elt == r_null) {
        dots = r_node_cdr(dots);
        r_node_poke_cdr(prev, dots);
        continue;
      }

      r_node_poke_cdr(prev, elt);

      r_obj* next = r_node_cdr(dots);
      r_obj* tail = r_pairlist_tail(elt);
      r_node_poke_cdr(tail, next);

      prev = tail;
      dots = next;
      continue;
    }

    prev = dots;
    dots = r_node_cdr(dots);
  }

  FREE(1);
  return r_node_cdr(out);
}


static r_obj* dots_keep(r_obj* dots, r_obj* nms, bool first) {
  r_ssize n = r_length(dots);

  r_obj* dups = KEEP(nms_are_duplicated(nms, !first));
  r_ssize out_n = n - r_lgl_sum(dups, false);

  r_obj* out = KEEP(r_alloc_list(out_n));
  r_obj* out_nms = KEEP(r_alloc_character(out_n));
  r_attrib_push(out, r_syms.names, out_nms);

  r_obj* const * p_nms = r_chr_cbegin(nms);
  const int* p_dups = r_lgl_cbegin(dups);

  for (r_ssize i = 0, out_i = 0; i < n; ++i) {
    if (!p_dups[i]) {
      r_list_poke(out, out_i, r_list_get(dots, i));
      r_chr_poke(out_nms, out_i, p_nms[i]);
      ++out_i;
    }
  }

  FREE(3);
  return out;
}

static r_obj* abort_dots_homonyms_call = NULL;
static void dots_check_homonyms(r_obj* dots, r_obj* nms) {
  r_obj* dups = KEEP(nms_are_duplicated(nms, false));

  if (r_lgl_sum(dups, false)) {
    r_eval_with_xy(abort_dots_homonyms_call, dots, dups, r_base_env);
    r_abort("Internal error: `dots_check_homonyms()` should have failed earlier");
  }

  FREE(1);
}


// From capture.c
r_obj* capturedots(r_obj* frame);

static r_obj* dots_capture(struct dots_capture_info* capture_info, r_obj* frame_env) {
  r_obj* dots = KEEP(capturedots(frame_env));
  dots = dots_unquote(dots, capture_info);
  FREE(1);
  return dots;
}

r_obj* rlang_unescape_character(r_obj*);

static r_obj* dots_finalise(struct dots_capture_info* capture_info, r_obj* dots) {
  r_obj* nms = r_names(dots);

  if (capture_info->type == DOTS_COLLECT_value && should_auto_name(capture_info->named)) {
    if (nms == r_null) {
      nms = r_alloc_character(r_length(dots));
    }
  }
  KEEP(nms);

  if (nms != r_null) {
    // Serialised unicode points might arise when unquoting lists
    // because of the conversion to pairlist
    nms = KEEP(rlang_unescape_character(nms));
    r_attrib_poke_names(dots, nms);

    dots = KEEP(maybe_auto_name(dots, capture_info->named));

    switch (capture_info->homonyms) {
    case DOTS_HOMONYMS_keep: break;
    case DOTS_HOMONYMS_first: dots = dots_keep(dots, nms, true); break;
    case DOTS_HOMONYMS_last: dots = dots_keep(dots, nms, false); break;
    case DOTS_HOMONYMS_error: dots_check_homonyms(dots, nms); break;
    }

    FREE(2);
  }

  FREE(1);
  return dots;
}


r_obj* rlang_exprs_interp(r_obj* frame_env,
                          r_obj* named,
                          r_obj* ignore_empty,
                          r_obj* unquote_names,
                          r_obj* homonyms,
                          r_obj* check_assign) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_COLLECT_expr,
                                   named,
                                   ignore_empty,
                                   r_true,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   true);

  r_obj* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));
  dots = KEEP(dots_as_list(dots, &capture_info));
  dots = dots_finalise(&capture_info, dots);

  FREE(2);
  return dots;
}
r_obj* rlang_quos_interp(r_obj* frame_env,
                         r_obj* named,
                         r_obj* ignore_empty,
                         r_obj* unquote_names,
                         r_obj* homonyms,
                         r_obj* check_assign) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_COLLECT_quo,
                                   named,
                                   ignore_empty,
                                   r_true,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   true);

  r_obj* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));
  dots = KEEP(dots_as_list(dots, &capture_info));
  dots = KEEP(dots_finalise(&capture_info, dots));

  r_obj* attrib = KEEP(r_new_node(r_names(dots), r_clone(quosures_attrib)));
  r_node_poke_tag(attrib, r_syms.names);
  r_poke_attrib(dots, attrib);
  r_mark_object(dots);

  FREE(4);
  return dots;
}

static bool is_spliced_bare_dots_value(r_obj* x) {
  if (r_typeof(x) != R_TYPE_list) {
    return false;
  }
  if (is_splice_box(x)) {
    return true;
  }
  if (r_is_object(x)) {
    return false;
  }
  return true;
}

static r_obj* dots_values_impl(r_obj* frame_env,
                               r_obj* named,
                               r_obj* ignore_empty,
                               r_obj* preserve_empty,
                               r_obj* unquote_names,
                               r_obj* homonyms,
                               r_obj* check_assign,
                               bool splice) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_COLLECT_value,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   splice);
  r_obj* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));

  if (capture_info.needs_expansion) {
    dots = KEEP(dots_as_list(dots, &capture_info));
  } else {
    dots = KEEP(r_vec_coerce(dots, R_TYPE_list));
  }

  dots = dots_finalise(&capture_info, dots);

  FREE(2);
  return dots;
}

r_obj* rlang_ext_dots_values(r_obj* args) {
  args = r_node_cdr(args);

  r_obj* env =            r_node_car(args); args = r_node_cdr(args);
  r_obj* named =          r_node_car(args); args = r_node_cdr(args);
  r_obj* ignore_empty =   r_node_car(args); args = r_node_cdr(args);
  r_obj* preserve_empty = r_node_car(args); args = r_node_cdr(args);
  r_obj* unquote_names =  r_node_car(args); args = r_node_cdr(args);
  r_obj* homonyms =       r_node_car(args); args = r_node_cdr(args);
  r_obj* check_assign =   r_node_car(args);

  r_obj* out = dots_values_impl(env,
                                named,
                                ignore_empty,
                                preserve_empty,
                                unquote_names,
                                homonyms,
                                check_assign,
                                false);

  return out;
}
r_obj* rlang_env_dots_values(r_obj* env) {
  return dots_values_impl(env,
                          r_false,
                          rlang_objs_trailing,
                          r_false,
                          r_true,
                          rlang_objs_keep,
                          r_false,
                          false);
}
r_obj* rlang_env_dots_list(r_obj* env) {
  return dots_values_impl(env,
                          r_false,
                          rlang_objs_trailing,
                          r_false,
                          r_true,
                          rlang_objs_keep,
                          r_false,
                          true);
}

r_obj* rlang_dots_list(r_obj* frame_env,
                       r_obj* named,
                       r_obj* ignore_empty,
                       r_obj* preserve_empty,
                       r_obj* unquote_names,
                       r_obj* homonyms,
                       r_obj* check_assign) {
  return dots_values_impl(frame_env,
                          named,
                          ignore_empty,
                          preserve_empty,
                          unquote_names,
                          homonyms,
                          check_assign,
                          true);
}
r_obj* rlang_dots_flat_list(r_obj* frame_env,
                            r_obj* named,
                            r_obj* ignore_empty,
                            r_obj* preserve_empty,
                            r_obj* unquote_names,
                            r_obj* homonyms,
                            r_obj* check_assign) {

  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_COLLECT_value,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   true);

  r_obj* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));
  dots = KEEP(r_vec_coerce(dots, R_TYPE_list));

  dots = KEEP(r_squash_if(dots, R_TYPE_list, is_spliced_bare_dots_value, 1));
  dots = dots_finalise(&capture_info, dots);

  FREE(3);
  return dots;
}

r_obj* dots_values_node_impl(r_obj* frame_env,
                             r_obj* named,
                             r_obj* ignore_empty,
                             r_obj* preserve_empty,
                             r_obj* unquote_names,
                             r_obj* homonyms,
                             r_obj* check_assign,
                             bool splice) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_COLLECT_value,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce_pairlist,
                                   splice);

  r_obj* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));

  dots = KEEP(dots_as_pairlist(dots, &capture_info));

  // dots = dots_finalise(&capture_info, dots);

  FREE(2);
  return dots;
}
r_obj* rlang_dots_pairlist(r_obj* frame_env,
                           r_obj* named,
                           r_obj* ignore_empty,
                           r_obj* preserve_empty,
                           r_obj* unquote_names,
                           r_obj* homonyms,
                           r_obj* check_assign) {
  return dots_values_node_impl(frame_env,
                               named,
                               ignore_empty,
                               preserve_empty,
                               unquote_names,
                               homonyms,
                               check_assign,
                               true);
}

void rlang_init_dots(r_obj* ns) {
  glue_unquote_fn = r_eval(r_sym("glue_unquote"), ns);

  auto_name_call = r_parse("rlang:::quos_auto_name(x)");
  r_preserve(auto_name_call);

  abort_dots_homonyms_call = r_parse("rlang:::abort_dots_homonyms(x, y)");
  r_preserve(abort_dots_homonyms_call);

  {
    r_obj* splice_box_class = KEEP(r_alloc_character(2));
    r_chr_poke(splice_box_class, 0, r_str("rlang_box_splice"));
    r_chr_poke(splice_box_class, 1, r_str("rlang_box"));

    splice_box_attrib = r_pairlist(splice_box_class);
    r_preserve(splice_box_attrib);
    r_mark_shared(splice_box_attrib);

    r_node_poke_tag(splice_box_attrib, r_syms.class);
    FREE(1);
  }

  {
    r_obj* list = KEEP(r_alloc_list(0));
    empty_spliced_arg = rlang_new_splice_box(list);
    r_preserve(empty_spliced_arg);
    r_mark_shared(empty_spliced_arg);
    FREE(1);
  }

  {
    r_obj* quosures_class = KEEP(r_alloc_character(2));
    r_chr_poke(quosures_class, 0, r_str("quosures"));
    r_chr_poke(quosures_class, 1, r_str("list"));

    quosures_attrib = r_pairlist(quosures_class);
    r_preserve(quosures_attrib);
    r_mark_shared(quosures_attrib);

    r_node_poke_tag(quosures_attrib, r_syms.class);
    FREE(1);
  }

  as_label_call = r_parse("as_label(x)");
  r_preserve(as_label_call);
}
