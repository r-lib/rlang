#include <rlang.h>
#include "dots.h"
#include "expr-interp.h"
#include "internal.h"
#include "utils.h"

sexp* rlang_ns_get(const char* name);

// Initialised at load time
static sexp* empty_spliced_arg = NULL;
static sexp* splice_box_attrib = NULL;

sexp* rlang_new_splice_box(sexp* x) {
  sexp* out = KEEP(r_new_vector(r_type_list, 1));
  r_list_poke(out, 0, x);
  r_poke_attributes(out, splice_box_attrib);
  r_mark_object(out);
  FREE(1);
  return out;
}
bool is_splice_box(sexp* x) {
  return r_get_attributes(x) == splice_box_attrib;
}
sexp* rlang_is_splice_box(sexp* x) {
  return r_lgl(is_splice_box(x));
}
sexp* rlang_unbox(sexp* x) {
  if (r_length(x) != 1) {
    r_abort("Internal error: Expected a list of size 1 in `rlang_unbox()`.");
  }
  return r_list_get(x, 0);
}


enum dots_homonyms {
  DOTS_HOMONYMS_KEEP = 0,
  DOTS_HOMONYMS_FIRST,
  DOTS_HOMONYMS_LAST,
  DOTS_HOMONYMS_ERROR
};

struct dots_capture_info {
  enum dots_capture_type type;
  r_ssize count;
  sexp* named;
  bool needs_expansion;
  int ignore_empty;
  bool preserve_empty;
  bool unquote_names;
  enum dots_homonyms homonyms;
  bool check_assign;
  sexp* (*big_bang_coerce)(sexp*);
  bool splice;
};

static int arg_match_ignore_empty(sexp* ignore_empty);
static enum dots_homonyms arg_match_homonyms(sexp* homonyms);

struct dots_capture_info init_capture_info(enum dots_capture_type type,
                                           sexp* named,
                                           sexp* ignore_empty,
                                           sexp* preserve_empty,
                                           sexp* unquote_names,
                                           sexp* homonyms,
                                           sexp* check_assign,
                                           sexp* (*coercer)(sexp*),
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


static sexp* def_unquote_name(sexp* expr, sexp* env) {
  int n_kept = 0;
  sexp* lhs = r_node_cadr(expr);

  struct expansion_info info = which_expansion_op(lhs, true);

  switch (info.op) {
  case OP_EXPAND_NONE:
    break;
  case OP_EXPAND_UQ:
    lhs = KEEP_N(r_eval(info.operand, env), n_kept);
    break;
  case OP_EXPAND_CURLY:
    lhs = KEEP_N(rlang_enquo(info.operand, env), n_kept);
    break;
  case OP_EXPAND_UQS:
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
  case OP_EXPAND_UQN:
    r_abort("Internal error: Chained `:=` should have been detected earlier");
  case OP_EXPAND_FIXUP:
    r_abort("The LHS of `:=` must be a string or a symbol");
  case OP_EXPAND_DOT_DATA:
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
    r_warn_deprecated(msg, msg);
}

static sexp* dots_big_bang_coerce(sexp* x) {
  switch (r_typeof(x)) {
  case r_type_null:
  case r_type_pairlist:
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
    if (r_is_object(x)) {
      return r_eval_with_x(as_list_call, r_global_env, x);
    } else {
      return r_vec_coerce(x, r_type_list);
    }
  case r_type_list:
    if (r_is_object(x)) {
      return r_eval_with_x(as_list_call, r_global_env, x);
    } else {
      return x;
    }
  case r_type_s4:
    return r_eval_with_x(as_list_s4_call, r_methods_ns_env, x);
  case r_type_call:
    if (r_is_symbol(r_node_car(x), "{")) {
      return r_vec_coerce(r_node_cdr(x), r_type_list);
    }
    // else fallthrough
  case r_type_symbol:
    signal_retired_splice();
    return r_new_list(x, NULL);

  default:
    r_abort(
      "Can't splice an object of type `%s` because it is not a vector",
      r_type_as_c_string(r_typeof(x))
    );
  }
}

// Also used in expr-interp.c
sexp* big_bang_coerce_pairlist(sexp* x, bool deep) {
  int n_protect = 0;

  if (r_is_object(x)) {
    x = KEEP_N(dots_big_bang_coerce(x), n_protect);
  }

  switch (r_typeof(x)) {
  case r_type_null:
  case r_type_pairlist:
    x = r_duplicate(x, true);
    break;
  case r_type_logical:
  case r_type_integer:
  case r_type_double:
  case r_type_complex:
  case r_type_character:
  case r_type_raw:
  case r_type_list:
    x = r_vec_coerce(x, r_type_pairlist);
    break;
  case r_type_call:
    if (deep && r_is_symbol(r_node_car(x), "{")) {
      x = r_node_cdr(x);
      break;
    }
    // fallthrough
  case r_type_symbol: {
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

  FREE(n_protect);
  return x;
}
static sexp* dots_big_bang_coerce_pairlist(sexp* x) {
  return big_bang_coerce_pairlist(x, false);
}

static sexp* dots_big_bang_value(struct dots_capture_info* capture_info,
                                 sexp* value, sexp* env, bool quosured) {
  value = KEEP(capture_info->big_bang_coerce(value));

  r_ssize n = r_length(value);

  if (quosured) {
    if (r_is_shared(value)) {
      sexp* tmp = r_duplicate(value, true);
      FREE(1);
      value = KEEP(tmp);
    }

    for (r_ssize i = 0; i < n; ++i) {
      sexp* elt = r_list_get(value, i);
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
static sexp* dots_big_bang(struct dots_capture_info* capture_info,
                           sexp* expr, sexp* env, bool quosured) {
  sexp* value = KEEP(r_eval(expr, env));
  sexp* out = dots_big_bang_value(capture_info, value, env, quosured);
  FREE(1);
  return out;
}

static inline bool should_ignore(int ignore_empty, r_ssize i, r_ssize n) {
  return ignore_empty == 1 || (i == n - 1 && ignore_empty == -1);
}
static inline sexp* dot_get_expr(sexp* dot) {
  return r_list_get(dot, 0);
}
static inline sexp* dot_get_env(sexp* dot) {
  return r_list_get(dot, 1);
}

static sexp* dots_unquote(sexp* dots, struct dots_capture_info* capture_info) {
  capture_info->count = 0;
  r_ssize n = r_length(dots);
  bool unquote_names = capture_info->unquote_names;

  sexp* node = dots;
  for (r_ssize i = 0; node != r_null; ++i, node = r_node_cdr(node)) {
    sexp* elt = r_node_car(node);
    sexp* expr = dot_get_expr(elt);
    sexp* env = dot_get_env(elt);

    // Unquoting rearranges expressions
    expr = KEEP(r_duplicate(expr, false));

    if (unquote_names && r_is_call(expr, ":=")) {
      if (r_node_tag(node) != r_null) {
        r_abort("Can't supply both `=` and `:=`");
      }

      sexp* nm = def_unquote_name(expr, env);
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
    enum dots_expansion_op dots_op = info.op + (EXPANSION_OP_MAX * capture_info->type);

    sexp* name = r_node_tag(node);

    // Ignore empty arguments
    if (expr == r_missing_sym
        && (name == r_null || name == r_empty_str)
        && should_ignore(capture_info->ignore_empty, i, n)) {
      capture_info->needs_expansion = true;
      r_node_poke_car(node, empty_spliced_arg);
      FREE(1);
      continue;
    }

    switch (dots_op) {
    case OP_EXPR_NONE:
    case OP_EXPR_UQ:
    case OP_EXPR_FIXUP:
    case OP_EXPR_DOT_DATA:
    case OP_EXPR_CURLY:
      expr = call_interp_impl(expr, env, info);
      capture_info->count += 1;
      break;
    case OP_EXPR_UQS:
      expr = dots_big_bang(capture_info, info.operand, env, false);
      break;
    case OP_QUO_NONE:
    case OP_QUO_UQ:
    case OP_QUO_FIXUP:
    case OP_QUO_DOT_DATA:
    case OP_QUO_CURLY: {
      expr = KEEP(call_interp_impl(expr, env, info));
      expr = forward_quosure(expr, env);
      FREE(1);
      capture_info->count += 1;
      break;
    }
    case OP_QUO_UQS: {
      expr = dots_big_bang(capture_info, info.operand, env, true);
      break;
    }
    case OP_VALUE_NONE:
    case OP_VALUE_FIXUP:
    case OP_VALUE_DOT_DATA:
      if (expr == r_missing_sym) {
        if (!capture_info->preserve_empty) {
          r_abort("Argument %d is empty", i + 1);
        }
      } else if (env != r_empty_env) {
        // Don't evaluate when `env` is the empty environment. This
        // happens when the argument was forced (and thus already
        // evaluated), for instance by lapply() or map().
        expr = r_eval(expr, env);
      }
      if (is_splice_box(expr)) {
        // Coerce contents of splice boxes to ensure uniform type
        KEEP(expr);
        expr = rlang_unbox(expr);
        expr = dots_big_bang_value(capture_info, expr, env, false);
        FREE(1);
      } else {
        capture_info->count += 1;
      }
      break;
    case OP_VALUE_UQ:
      r_abort("Can't use `!!` in a non-quoting function");
    case OP_VALUE_UQS: {
      expr = KEEP(r_eval(info.operand, env));
      expr = dots_big_bang(capture_info, info.operand, env, false);
      FREE(1);
      break;
    }
    case OP_VALUE_CURLY:
      r_abort("Can't use `{{` in a non-quoting function");
    case OP_EXPR_UQN:
    case OP_QUO_UQN:
    case OP_VALUE_UQN:
      r_abort("`:=` can't be chained");
    case OP_DOTS_MAX:
      r_abort("Internal error: `OP_DOTS_MAX`");
    }

    r_node_poke_car(node, expr);
    FREE(1);
  }

  return dots;
}


static int arg_match_ignore_empty(sexp* ignore_empty) {
  if (r_typeof(ignore_empty) != r_type_character || r_length(ignore_empty) == 0) {
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

static enum dots_homonyms arg_match_homonyms(sexp* homonyms) {
  if (r_typeof(homonyms) != r_type_character || r_length(homonyms) == 0) {
    r_abort("`.homonyms` must be a character vector");
  }
  const char* arg = r_chr_get_c_string(homonyms, 0);
  switch(arg[0]) {
  case 'k': if (!strcmp(arg, "keep")) return DOTS_HOMONYMS_KEEP; else break;
  case 'f': if (!strcmp(arg, "first")) return DOTS_HOMONYMS_FIRST; else break;
  case 'l': if (!strcmp(arg, "last")) return DOTS_HOMONYMS_LAST; else break;
  case 'e': if (!strcmp(arg, "error")) return DOTS_HOMONYMS_ERROR; else break;
  }
  r_abort("`.homonyms` must be one of: \"keep\", \"first\", \"last\", or \"error\"");
}

static void warn_deprecated_width() {
  const char* msg = "`.named` can no longer be a width";
  r_warn_deprecated(msg, msg);
}
static bool should_auto_name(sexp* named) {
  if (r_length(named) != 1) {
    goto error;
  }

  switch (r_typeof(named)) {
  case r_type_logical:
    return r_lgl_get(named, 0);
  case r_type_integer:
    warn_deprecated_width();
    return INTEGER(named)[0];
  case r_type_double:
    if (r_is_integerish(named, -1, true)) {
      warn_deprecated_width();
      return REAL(named)[0];
    }
    // else fallthrough
  default:
    break;
  }

 error:
  r_abort("`.named` must be a scalar logical");
}

static sexp* auto_name_call = NULL;

static sexp* maybe_auto_name(sexp* x, sexp* named) {
  sexp* names = r_vec_names(x);

  if (should_auto_name(named) && (names == r_null || r_chr_has(names, ""))) {
    x = r_eval_with_x(auto_name_call, r_base_env, x);
  }

  return x;
}

static bool any_name(sexp* x, bool splice) {
  while (x != r_null) {
    if (r_node_tag(x) != r_null) {
      return true;
    }

    sexp* elt = r_node_car(x);

    if (splice && is_splice_box(elt)) {
      if (r_vec_names(rlang_unbox(elt)) != r_null) {
        return true;
      }
    }

    x = r_node_cdr(x);
  }

  return false;
}

static void check_named_splice(sexp* node) {
  if (r_node_tag(node) != r_null) {
    const char* msg = "`!!!` can't be supplied with a name. Only the operand's names are retained.";
    r_stop_defunct(msg);
  }
}

sexp* dots_as_list(sexp* dots, struct dots_capture_info* capture_info) {
  int n_protect = 0;

  sexp* out = KEEP_N(r_new_vector(r_type_list, capture_info->count), n_protect);

  // Add default empty names unless dots are captured by values
  sexp* out_names = r_null;
  if (capture_info->type != DOTS_VALUE || any_name(dots, capture_info->splice)) {
    out_names = KEEP_N(r_new_vector(r_type_character, capture_info->count), n_protect);
    r_push_names(out, out_names);
  }

  for (r_ssize i = 0, count = 0; dots != r_null; ++i, dots = r_node_cdr(dots)) {
    sexp* elt = r_node_car(dots);

    if (elt == empty_spliced_arg) {
      continue;
    }

    if (capture_info->splice && is_splice_box(elt)) {
      check_named_splice(dots);

      elt = rlang_unbox(elt);
      sexp* nms = r_vec_names(elt);

      r_ssize n = r_length(elt);
      for (r_ssize i = 0; i < n; ++i) {
        sexp* value = r_list_get(elt, i);
        r_list_poke(out, count, value);

        sexp* name = r_nms_get(nms, i);
        if (name != r_empty_str) {
          r_chr_poke(out_names, count, name);
        }

        ++count;
      }
    } else {
      r_list_poke(out, count, elt);

      sexp* name = r_node_tag(dots);
      if (name != r_null) {
        r_chr_poke(out_names, count, r_string(r_sym_get_c_string(name)));
      }

      ++count;
    }
  }

  FREE(n_protect);
  return out;
}

sexp* dots_as_pairlist(sexp* dots, struct dots_capture_info* capture_info) {
  sexp* out = KEEP(r_new_node(r_null, dots));
  sexp* prev = out;

  while (dots != r_null) {
    sexp* elt = r_node_car(dots);

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

      sexp* next = r_node_cdr(dots);
      sexp* tail = r_node_tail(elt);
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


static sexp* dots_keep(sexp* dots, sexp* nms, bool first) {
  r_ssize n = r_length(dots);

  sexp* dups = KEEP(r_nms_are_duplicated(nms, !first));
  r_ssize out_n = n - r_lgl_sum(dups, false);

  sexp* out = KEEP(r_new_vector(r_type_list, out_n));
  sexp* out_nms = KEEP(r_new_vector(r_type_character, out_n));
  r_push_names(out, out_nms);

  sexp** nms_ptr = r_chr_deref(nms);
  int* dups_ptr = r_lgl_deref(dups);

  for (r_ssize i = 0, out_i = 0; i < n; ++i, ++nms_ptr, ++dups_ptr) {
    if (!*dups_ptr) {
      r_list_poke(out, out_i, r_list_get(dots, i));
      r_chr_poke(out_nms, out_i, *nms_ptr);
      ++out_i;
    }
  }

  FREE(3);
  return out;
}

static sexp* abort_dots_homonyms_call = NULL;
static void dots_check_homonyms(sexp* dots, sexp* nms) {
  sexp* dups = KEEP(r_nms_are_duplicated(nms, false));

  if (r_lgl_sum(dups, false)) {
    r_eval_with_xy(abort_dots_homonyms_call, r_base_env, dots, dups);
    r_abort("Internal error: `dots_check_homonyms()` should have failed earlier");
  }

  FREE(1);
}


// From capture.c
sexp* capturedots(sexp* frame);

static sexp* dots_capture(struct dots_capture_info* capture_info, sexp* frame_env) {
  sexp* dots = KEEP(capturedots(frame_env));
  dots = dots_unquote(dots, capture_info);
  FREE(1);
  return dots;
}

sexp* rlang_unescape_character(sexp*);

static sexp* dots_finalise(struct dots_capture_info* capture_info, sexp* dots) {
  sexp* nms = r_vec_names(dots);

  if (nms != r_null) {
    // Serialised unicode points might arise when unquoting lists
    // because of the conversion to pairlist
    nms = KEEP(rlang_unescape_character(nms));
    r_poke_names(dots, nms);

    dots = KEEP(maybe_auto_name(dots, capture_info->named));

    switch (capture_info->homonyms) {
    case DOTS_HOMONYMS_KEEP: break;
    case DOTS_HOMONYMS_FIRST: dots = dots_keep(dots, nms, true); break;
    case DOTS_HOMONYMS_LAST: dots = dots_keep(dots, nms, false); break;
    case DOTS_HOMONYMS_ERROR: dots_check_homonyms(dots, nms); break;
    }

    FREE(2);
  }

  return dots;
}


sexp* rlang_exprs_interp(sexp* frame_env,
                         sexp* named,
                         sexp* ignore_empty,
                         sexp* unquote_names,
                         sexp* homonyms,
                         sexp* check_assign) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_EXPR,
                                   named,
                                   ignore_empty,
                                   r_shared_true,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   true);

  sexp* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));
  dots = KEEP(dots_as_list(dots, &capture_info));
  dots = dots_finalise(&capture_info, dots);

  FREE(2);
  return dots;
}
sexp* rlang_quos_interp(sexp* frame_env,
                        sexp* named,
                        sexp* ignore_empty,
                        sexp* unquote_names,
                        sexp* homonyms,
                        sexp* check_assign) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_QUO,
                                   named,
                                   ignore_empty,
                                   r_shared_true,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   true);

  sexp* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));
  dots = KEEP(dots_as_list(dots, &capture_info));
  dots = KEEP(dots_finalise(&capture_info, dots));

  r_push_class(dots, "quosures");

  FREE(3);
  return dots;
}

static bool is_spliced_bare_dots_value(sexp* x) {
  if (r_typeof(x) != r_type_list) {
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

static sexp* dots_values_impl(sexp* frame_env,
                              sexp* named,
                              sexp* ignore_empty,
                              sexp* preserve_empty,
                              sexp* unquote_names,
                              sexp* homonyms,
                              sexp* check_assign,
                              bool splice) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_VALUE,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   splice);
  sexp* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));

  if (capture_info.needs_expansion) {
    dots = KEEP(dots_as_list(dots, &capture_info));
  } else {
    dots = KEEP(r_vec_coerce(dots, r_type_list));
  }

  dots = dots_finalise(&capture_info, dots);

  FREE(2);
  return dots;
}

sexp* rlang_ext2_dots_values(sexp* call,
                        sexp* op,
                        sexp* args,
                        sexp* env) {
  args = r_node_cdr(args);

  sexp* named =          KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  sexp* ignore_empty =   KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  sexp* preserve_empty = KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  sexp* unquote_names =  KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  sexp* homonyms =       KEEP(r_eval(r_node_car(args), env)); args = r_node_cdr(args);
  sexp* check_assign =   KEEP(r_eval(r_node_car(args), env));

  sexp* out = dots_values_impl(env,
                               named,
                               ignore_empty,
                               preserve_empty,
                               unquote_names,
                               homonyms,
                               check_assign,
                               false);

  FREE(6);
  return out;
}
sexp* rlang_env_dots_values(sexp* env) {
  return dots_values_impl(env,
                          r_shared_false,
                          rlang_objs_trailing,
                          r_shared_false,
                          r_shared_true,
                          rlang_objs_keep,
                          r_shared_false,
                          false);
}
sexp* rlang_env_dots_list(sexp* env) {
  return dots_values_impl(env,
                          r_shared_false,
                          rlang_objs_trailing,
                          r_shared_false,
                          r_shared_true,
                          rlang_objs_keep,
                          r_shared_false,
                          true);
}

sexp* rlang_dots_list(sexp* frame_env,
                      sexp* named,
                      sexp* ignore_empty,
                      sexp* preserve_empty,
                      sexp* unquote_names,
                      sexp* homonyms,
                      sexp* check_assign) {
  return dots_values_impl(frame_env,
                          named,
                          ignore_empty,
                          preserve_empty,
                          unquote_names,
                          homonyms,
                          check_assign,
                          true);
}
sexp* rlang_dots_flat_list(sexp* frame_env,
                           sexp* named,
                           sexp* ignore_empty,
                           sexp* preserve_empty,
                           sexp* unquote_names,
                           sexp* homonyms,
                           sexp* check_assign) {

  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_VALUE,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce,
                                   true);

  sexp* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));
  dots = KEEP(r_vec_coerce(dots, r_type_list));

  dots = KEEP(r_squash_if(dots, r_type_list, is_spliced_bare_dots_value, 1));
  dots = dots_finalise(&capture_info, dots);

  FREE(3);
  return dots;
}

sexp* dots_values_node_impl(sexp* frame_env,
                            sexp* named,
                            sexp* ignore_empty,
                            sexp* preserve_empty,
                            sexp* unquote_names,
                            sexp* homonyms,
                            sexp* check_assign,
                            bool splice) {
  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_VALUE,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   homonyms,
                                   check_assign,
                                   &dots_big_bang_coerce_pairlist,
                                   splice);

  sexp* dots;
  dots = KEEP(dots_capture(&capture_info, frame_env));

  dots = KEEP(dots_as_pairlist(dots, &capture_info));

  // dots = dots_finalise(&capture_info, dots);

  FREE(2);
  return dots;
}
sexp* rlang_dots_pairlist(sexp* frame_env,
                          sexp* named,
                          sexp* ignore_empty,
                          sexp* preserve_empty,
                          sexp* unquote_names,
                          sexp* homonyms,
                          sexp* check_assign) {
  return dots_values_node_impl(frame_env,
                               named,
                               ignore_empty,
                               preserve_empty,
                               unquote_names,
                               homonyms,
                               check_assign,
                               true);
}

void rlang_init_dots() {
  auto_name_call = r_parse("rlang:::quos_auto_name(x)");
  r_mark_precious(auto_name_call);

  abort_dots_homonyms_call = r_parse("rlang:::abort_dots_homonyms(x, y)");
  r_mark_precious(abort_dots_homonyms_call);

  {
    sexp* splice_box_class = KEEP(r_new_vector(r_type_character, 2));
    r_chr_poke(splice_box_class, 0, r_string("rlang_box_splice"));
    r_chr_poke(splice_box_class, 1, r_string("rlang_box"));

    splice_box_attrib = r_pairlist(splice_box_class);
    r_mark_precious(splice_box_attrib);
    r_mark_shared(splice_box_attrib);

    r_node_poke_tag(splice_box_attrib, r_class_sym);
    FREE(1);
  }

  {
    sexp* list = KEEP(r_new_vector(r_type_list, 0));
    empty_spliced_arg = rlang_new_splice_box(list);
    r_mark_precious(empty_spliced_arg);
    r_mark_shared(empty_spliced_arg);
    FREE(1);
  }
}
