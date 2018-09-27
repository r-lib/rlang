#include <rlang.h>
#include "dots.h"
#include "expr-interp.h"
#include "internal.h"
#include "utils.h"

sexp* rlang_ns_get(const char* name);


struct dots_capture_info {
  enum dots_capture_type type;
  r_ssize count;
  sexp* named;
  bool needs_expansion;
  int ignore_empty;
  bool preserve_empty;
  bool unquote_names;
  bool check_assign;
};

static int match_ignore_empty_arg(sexp* ignore_empty);
static int find_auto_names_width(sexp* named);

struct dots_capture_info init_capture_info(enum dots_capture_type type,
                                           sexp* named,
                                           sexp* ignore_empty,
                                           sexp* preserve_empty,
                                           sexp* unquote_names,
                                           sexp* check_assign) {
  struct dots_capture_info info;

  info.type = type;
  info.count = 0;
  info.needs_expansion = false;
  info.named = named;
  info.ignore_empty = match_ignore_empty_arg(ignore_empty);
  info.preserve_empty = r_lgl_get(preserve_empty, 0);
  info.unquote_names = r_as_bool(unquote_names);
  info.check_assign = r_as_bool(check_assign);

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
  case OP_EXPAND_UQE:
    r_abort("The LHS of `:=` can't be unquoted with `UQE()`");
  case OP_EXPAND_UQS:
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
  case OP_EXPAND_UQN:
    r_abort("Internal error: Chained `:=` should have been detected earlier");
  case OP_EXPAND_FIXUP:
    r_abort("The LHS of `:=` must be a string or a symbol");
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

  sexp* name = r_sym_str(lhs);

  // Unserialise unicode points such as <U+xxx> that arise when
  // UTF-8 names are converted to symbols and the native encoding
  // does not support the characters (i.e. all the time on Windows)
  name = r_str_unserialise_unicode(name);

  FREE(n_kept);
  return name;
}


static sexp* rlang_spliced_flag = NULL;
static inline bool is_spliced_dots(sexp* x) {
  return r_get_attribute(x, rlang_spliced_flag) != r_null;
}
static inline void mark_spliced_dots(sexp* x) {
  r_poke_attribute(x, rlang_spliced_flag, rlang_spliced_flag);
}

void signal_retired_splice() {
  const char* msg =
    "Unquoting language objects with `!!!` is soft-deprecated as of rlang 0.3.0.\n"
    "Please use `!!` instead.\n"
    "\n"
    "  # Bad:\n"
    "  dplyr::select(data, !!!enquo(x))\n"
    "\n"
    "  # Good:\n"
    "  dplyr::select(data, !!enquo(x))    # Unquote single quosure\n"
    "  dplyr::select(data, !!!enquos(x))  # Splice list of quosures\n";
    r_signal_soft_deprecated(msg, msg, "rlang", r_empty_env);
}

// Maintain parity with deep_big_bang_coerce() in expr-interp.c
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
      return r_eval_with_x(as_list_call, r_base_env, x);
    } else {
      return r_vec_coerce(x, r_type_list);
    }
  case r_type_list:
    if (r_is_object(x)) {
      return r_eval_with_x(as_list_call, r_base_env, x);
    } else {
      return r_duplicate(x, true);
    }
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

static sexp* dots_value_big_bang(sexp* x) {
  x = KEEP(dots_big_bang_coerce(x));
  r_push_class(x, "spliced");

  FREE(1);
  return x;
}

static sexp* dots_big_bang(struct dots_capture_info* capture_info,
                           sexp* expr, sexp* env, bool quosured) {
  sexp* value = KEEP(r_eval(expr, env));
  value = KEEP(dots_big_bang_coerce(value));
  mark_spliced_dots(value);

  r_ssize n = r_length(value);
  capture_info->count += n;

  if (quosured) {
    for (r_ssize i = 0; i < n; ++i) {
      expr = r_list_get(value, i);
      expr = forward_quosure(expr, env);
      r_list_poke(value, i, expr);
    }
  }

  FREE(2);
  return value;
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

static sexp* empty_spliced_list() {
  static sexp* list = NULL;
  if (!list) {
    list = new_preserved_empty_list();
    mark_spliced_dots(list);
  }
  return list;
}

static sexp* dots_unquote(sexp* dots, struct dots_capture_info* capture_info) {
  if (!rlang_spliced_flag) rlang_spliced_flag = r_sym("__rlang_spliced");

  sexp* dots_names = r_vec_names(dots);
  capture_info->count = 0;
  r_ssize n = r_length(dots);
  bool unquote_names = capture_info->unquote_names;

  int i_protect;
  KEEP_WITH_INDEX(dots_names, i_protect);

  for (r_ssize i = 0; i < n; ++i) {
    sexp* elt = r_list_get(dots, i);
    sexp* expr = dot_get_expr(elt);
    sexp* env = dot_get_env(elt);

    // Unquoting rearranges expressions
    expr = KEEP(r_duplicate(expr, false));

    if (unquote_names && r_is_call(expr, ":=")) {
      sexp* name = KEEP(def_unquote_name(expr, env));

      if (dots_names == r_null) {
        dots_names = r_new_vector(r_type_character, n);
        KEEP_I(dots_names, i_protect);
        r_push_names(dots, dots_names);
      }

      if (r_chr_has_empty_string_at(dots_names, i)) {
        r_chr_poke(dots_names, i, name);
      } else {
        r_abort("Can't supply both `=` and `:=`");
      }
      expr = r_node_cadr(r_node_cdr(expr));

      FREE(1);
    }

    if (capture_info->check_assign
        && r_is_call(expr, "<-")
        && r_peek_option("rlang_dots_disable_assign_warning") == r_null) {
      r_warn(
        "Using `<-` as argument is often a mistake.\n"
        "Do you need to use `=` to match an argument?\n"
        "\n"
        "If you really want to use `<-`, please wrap in parentheses:\n"
        "\n"
        "  # Bad:\n"
        "  fn(a <- 1)\n"
        "\n"
        "  # Good:\n"
        "  fn(a = 1)     # Match 1 to parameter `a`\n"
        "  fn((a <- 1))  # Assign 1 to variable `a`"
      );
    }

    struct expansion_info info = which_expansion_op(expr, unquote_names);
    enum dots_expansion_op dots_op = info.op + (EXPANSION_OP_MAX * capture_info->type);

    // Ignore empty arguments
    if (expr == r_missing_sym
        && (dots_names == r_null || r_chr_has_empty_string_at(dots_names, i))
        && should_ignore(capture_info->ignore_empty, i, n)) {
      capture_info->needs_expansion = true;
      r_list_poke(dots, i, empty_spliced_list());
      FREE(1);
      continue;
    }

    switch (dots_op) {
    case OP_EXPR_NONE:
    case OP_EXPR_UQ:
    case OP_EXPR_UQE:
    case OP_EXPR_FIXUP:
      expr = call_interp_impl(expr, env, info);
      capture_info->count += 1;
      break;
    case OP_EXPR_UQS:
      capture_info->needs_expansion = true;
      expr = dots_big_bang(capture_info, info.operand, env, false);
      break;
    case OP_QUO_NONE:
    case OP_QUO_UQ:
    case OP_QUO_UQE:
    case OP_QUO_FIXUP: {
      expr = KEEP(call_interp_impl(expr, env, info));
      expr = forward_quosure(expr, env);
      FREE(1);
      capture_info->count += 1;
      break;
    }
    case OP_QUO_UQS: {
      capture_info->needs_expansion = true;
      expr = dots_big_bang(capture_info, info.operand, env, true);
      break;
    }
    case OP_VALUE_FIXUP:
    case OP_VALUE_NONE:
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
      if (r_inherits(expr, "spliced")) {
        capture_info->needs_expansion = true;
      }
      capture_info->count += 1;
      break;
    case OP_VALUE_UQ:
      r_abort("Can't use `!!` in a non-quoting function");
    case OP_VALUE_UQE:
      r_abort("Can't use `UQE()` in a non-quoting function");
    case OP_VALUE_UQS: {
      expr = KEEP(r_eval(info.operand, env));
      capture_info->needs_expansion = true;
      if (expr == r_null) {
        expr = empty_spliced_list();
      } else {
        expr = dots_value_big_bang(expr);
        capture_info->count += 1;
      }
      FREE(1);
      break;
    }
    case OP_EXPR_UQN:
    case OP_QUO_UQN:
    case OP_VALUE_UQN:
      r_abort("`:=` can't be chained");
    case OP_DOTS_MAX:
      r_abort("Internal error: `OP_DOTS_MAX`");
    }

    r_list_poke(dots, i, expr);
    FREE(1);
  }

  FREE(1);
  return dots;
}


static int match_ignore_empty_arg(sexp* ignore_empty) {
  if (r_typeof(ignore_empty) != r_type_character || r_length(ignore_empty) == 0) {
    r_abort("`.ignore_empty` must be a character vector");
  }
  const char* arg = r_chr_get_c_string(ignore_empty, 0);
  switch(arg[0]) {
  case 't': if (!strcmp(arg, "trailing")) return -1; else break;
  case 'n': if (!strcmp(arg, "none")) return 0; else break;
  case 'a': if (!strcmp(arg, "all")) return 1; else break;
  }
  r_abort("`.ignore_empty` should be one of: \"trailing\", \"none\" or \"all\"");
}

static int find_auto_names_width(sexp* named) {
  if (r_length(named) != 1) {
    goto error;
  }

  switch (r_typeof(named)) {
  case r_type_logical:
    if (r_as_bool(named)) {
      return 60;
    } else {
      return 0;
    }
  case r_type_integer:
    return INTEGER(named)[0];
  case r_type_double:
    if (r_is_integerish(named, -1, true)) {
      return REAL(named)[0];
    }
    // else fallthrough
  default:
    break;
  }

 error:
  r_abort("`.named` must be a scalar logical or number");
}

static sexp* auto_name_call = NULL;

static sexp* maybe_auto_name(sexp* x, sexp* named) {
  int names_width = find_auto_names_width(named);
  sexp* names = r_vec_names(x);

  if (names_width && (!names || r_chr_has(names, ""))) {
    sexp* width = KEEP(r_int(names_width));
    x = r_eval_with_xy(auto_name_call, r_base_env, x, width);
    FREE(1);
  }

  return x;
}

static sexp* init_names(sexp* x) {
  sexp* nms = KEEP(r_new_vector(r_type_character, r_length(x)));
  r_push_names(x, nms);
  FREE(1);
  return nms;
}


// From capture.c
sexp* capturedots(sexp* frame);

sexp* dots_expand(sexp* dots, struct dots_capture_info* capture_info) {
  sexp* dots_names = r_vec_names(dots);

  sexp* out = KEEP(r_new_vector(r_type_list, capture_info->count));

  // Add default empty names unless dots are captured by values
  sexp* out_names = r_null;
  if (capture_info->type != DOTS_VALUE || dots_names != r_null) {
    out_names = init_names(out);
  }

  r_ssize n = r_length(dots);
  for (r_ssize i = 0, count = 0; i < n; ++i) {
    sexp* elt = r_list_get(dots, i);

    if (is_spliced_dots(elt)) {
      sexp* names = r_vec_names(elt);

      // FIXME: Should be able to avoid conversion to list for node
      // lists and character vectors
      for (r_ssize i = 0; i < r_length(elt); ++i) {
        sexp* value = r_list_get(elt, i);
        r_list_poke(out, count, value);

        sexp* name = r_nms_get(names, i);
        if (name != r_string("")) {
          // Serialised unicode points might arise when unquoting
          // lists because of the conversion to pairlist
          name = KEEP(r_str_unserialise_unicode(name));

          // Names might not be initialised when dots are captured by value
          if (out_names == r_null) {
            out_names = init_names(out);
          }
          r_chr_poke(out_names, count, name);

          FREE(1);
        }

        ++count;
      }
    } else {
      r_list_poke(out, count, elt);

      if (dots_names != r_null) {
        sexp* name = r_chr_get(dots_names, i);
        r_chr_poke(out_names, count, name);
      }

      ++count;
    }
  }

  out = maybe_auto_name(out, capture_info->named);

  FREE(1);
  return out;
}

sexp* dots_init(struct dots_capture_info* capture_info, sexp* frame_env) {

  sexp* dots = KEEP(capturedots(frame_env));
  dots = dots_unquote(dots, capture_info);

  // Initialise the names only if there is no expansion to avoid
  // unnecessary allocation and auto-labelling
  if (!capture_info->needs_expansion) {
    if (capture_info->type != DOTS_VALUE && r_vec_names(dots) == r_null) {
      init_names(dots);
    }
    dots = maybe_auto_name(dots, capture_info->named);
  }

  FREE(1);
  return dots;
}

sexp* rlang_exprs_interp(sexp* frame_env,
                         sexp* named,
                         sexp* ignore_empty,
                         sexp* unquote_names,
                         sexp* check_assign) {

  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_EXPR,
                                   named,
                                   ignore_empty,
                                   r_shared_true,
                                   unquote_names,
                                   check_assign);

  sexp* dots = dots_init(&capture_info, frame_env);

  if (capture_info.needs_expansion) {
    KEEP(dots);
    dots = dots_expand(dots, &capture_info);
    FREE(1);
  }
  return dots;
}
sexp* rlang_quos_interp(sexp* frame_env,
                        sexp* named,
                        sexp* ignore_empty,
                        sexp* unquote_names,
                        sexp* check_assign) {
  int n_protect = 0;

  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_QUO,
                                   named,
                                   ignore_empty,
                                   r_shared_true,
                                   unquote_names,
                                   check_assign);

  sexp* dots = KEEP_N(dots_init(&capture_info, frame_env), n_protect);

  if (capture_info.needs_expansion) {
    dots = dots_expand(dots, &capture_info);
    KEEP_N(dots, n_protect);
  }
  r_push_class(dots, "quosures");

  FREE(n_protect);
  return dots;
}

static bool is_spliced_dots_value(sexp* x) {
  if (r_typeof(x) != r_type_list) {
    return false;
  }
  if (is_spliced_dots(x) || r_inherits(x, "spliced")) {
    return true;
  }
  return false;
}
static bool is_spliced_bare_dots_value(sexp* x) {
  if (r_typeof(x) != r_type_list) {
    return false;
  }
  if (is_spliced_dots(x)) {
    return true;
  }
  if (r_inherits(x, "spliced")) {
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
                              sexp* check_assign,
                              bool (*is_spliced)(sexp*)) {

  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_VALUE,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   check_assign);

  sexp* dots = dots_init(&capture_info, frame_env);

  KEEP(dots);
  if (capture_info.needs_expansion) {
    if (is_spliced) {
      dots = r_squash_if(dots, r_type_list, is_spliced, 1);
    } else {
      dots = dots_expand(dots, &capture_info);
    }
  }

  FREE(1);
  return dots;
}
sexp* rlang_dots_values(sexp* frame_env,
                        sexp* named,
                        sexp* ignore_empty,
                        sexp* preserve_empty,
                        sexp* unquote_names,
                        sexp* check_assign) {
  return dots_values_impl(frame_env,
                          named,
                          ignore_empty,
                          preserve_empty,
                          unquote_names,
                          check_assign,
                          NULL);
}
sexp* rlang_dots_list(sexp* frame_env,
                      sexp* named,
                      sexp* ignore_empty,
                      sexp* preserve_empty,
                      sexp* unquote_names,
                      sexp* check_assign) {
  return dots_values_impl(frame_env,
                          named,
                          ignore_empty,
                          preserve_empty,
                          unquote_names,
                          check_assign,
                          &is_spliced_dots_value);
}
sexp* rlang_dots_flat_list(sexp* frame_env,
                           sexp* named,
                           sexp* ignore_empty,
                           sexp* preserve_empty,
                           sexp* unquote_names,
                           sexp* check_assign) {

  struct dots_capture_info capture_info;
  capture_info = init_capture_info(DOTS_VALUE,
                                   named,
                                   ignore_empty,
                                   preserve_empty,
                                   unquote_names,
                                   check_assign);

  sexp* dots = dots_init(&capture_info, frame_env);

  KEEP(dots);
  dots = r_squash_if(dots, r_type_list, is_spliced_bare_dots_value, 1);

  FREE(1);
  return dots;
}

void rlang_init_dots() {
  auto_name_call = r_parse("rlang:::quos_auto_name(x, y)");
  r_mark_precious(auto_name_call);
}
