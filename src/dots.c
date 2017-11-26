#include "rlang/rlang.h"
#include "expr-interp.h"

sexp* rlang_ns_get(const char* name);

enum dots_capture_type {
  DOTS_EXPR,
  DOTS_QUO,
  DOTS_VALUE
};

#define N_EXPANSION_OPS 4

enum dots_expansion_op {
  OP_EXPR_NONE,
  OP_EXPR_UQ,
  OP_EXPR_UQE,
  OP_EXPR_UQS,
  OP_QUO_NONE,
  OP_QUO_UQ,
  OP_QUO_UQE,
  OP_QUO_UQS,
  OP_VALUE_NONE,
  OP_VALUE_UQ,
  OP_VALUE_UQE,
  OP_VALUE_UQS
};

struct dots_capture_info {
  enum dots_capture_type type;
  r_size_t count;
  int ignore_empty;
  bool unquote_names;
};

static int match_ignore_empty_arg(sexp* ignore_empty);
static int find_auto_names_width(sexp* named);

struct dots_capture_info init_capture_info(enum dots_capture_type type,
                                           sexp* ignore_empty,
                                           sexp* unquote_names) {
  struct dots_capture_info info;

  info.type = type;
  info.count = 0;
  info.ignore_empty = match_ignore_empty_arg(ignore_empty);
  info.unquote_names = r_as_bool(unquote_names);

  return info;
}


static inline sexp* dot_get_expr(sexp* dot) {
  return r_list_get(dot, 0);
}
static inline sexp* dot_get_env(sexp* dot) {
  return r_list_get(dot, 1);
}
static inline void dot_poke_expr(sexp* dot, sexp* elt) {
  r_list_poke(dot, 0, elt);
}

static sexp* new_preserved_empty_list() {
  sexp* empty_list = r_new_vector(r_type_list, 0);
  r_mark_precious(empty_list);
  r_mark_shared(empty_list);

  sexp* nms = KEEP(r_new_vector(r_type_character, 0));
  r_poke_names(empty_list, nms);
  FREE(1);

  return empty_list;
}
static sexp* empty_named_list() {
  static sexp* empty_list = NULL;
  if (!empty_list) {
    empty_list = new_preserved_empty_list();
  }
  return empty_list;
}
static sexp* empty_quosures() {
  static sexp* empty_quos = NULL;
  if (!empty_quos) {
    empty_quos = new_preserved_empty_list();
    r_push_class(empty_quos, "quosures");
  }
  return empty_quos;
}

static sexp* def_unquote_name(sexp* expr, sexp* env) {
  int n_kept = 0;
  sexp* lhs = r_node_cadr(expr);

  struct expansion_info info = which_expansion_op(lhs);

  switch (info.op) {
  case OP_EXPAND_NONE:
    break;
  case OP_EXPAND_UQ:
    lhs = KEEP(r_eval(info.operand, env));
    ++n_kept;
    break;
  case OP_EXPAND_UQE:
    r_abort("The LHS of `:=` can't be unquoted with `UQE()`");
  case OP_EXPAND_UQS:
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
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
static sexp* rlang_ignored_flag = NULL;

static inline bool is_spliced_dots(sexp* x) {
  return r_get_attribute(x, rlang_spliced_flag) != r_null;
}
static inline void mark_spliced_dots(sexp* x) {
  r_poke_attribute(x, rlang_spliced_flag, rlang_spliced_flag);
}
static inline bool is_ignored_dot(sexp* x) {
  return r_get_attribute(x, rlang_ignored_flag) != r_null;
}
static inline void mark_ignored_dot(sexp* x) {
  r_poke_attribute(x, rlang_ignored_flag, rlang_ignored_flag);
}

static sexp* dots_big_bang(struct dots_capture_info* capture_info,
                           sexp* expr, sexp* env, bool quosured) {
  sexp* spliced_node = KEEP(r_eval(expr, env));
  spliced_node = big_bang_coerce(spliced_node);

  sexp* node = spliced_node;
  while (node != r_null) {
    expr = r_node_car(node);
    if (quosured) {
      expr = forward_quosure(expr, env);
    }
    r_node_poke_car(node, expr);

    node = r_node_cdr(node);
    capture_info->count += 1;
  }

  FREE(1);
  return spliced_node;
}

static inline bool should_ignore(int ignore_empty, r_size_t i, r_size_t n) {
  return ignore_empty == 1 || (i == n - 1 && ignore_empty == -1);
}

static sexp* set_value_spliced(sexp* x) {
  static sexp* spliced_str = NULL;
  if (!spliced_str) {
    spliced_str = r_scalar_chr("spliced");
    r_mark_precious(spliced_str);
    r_mark_shared(spliced_str);
  }

  if (r_typeof(x) != r_type_list) {
    r_abort("Can't use `!!!` on atomic vectors in non-quoting functions");
  }

  return r_set_attribute(x, r_class_sym, spliced_str);
}

static sexp* dots_unquote(sexp* dots, struct dots_capture_info* capture_info) {
  sexp* dots_names = r_names(dots);
  capture_info->count = 0;
  r_size_t n = r_length(dots);

  for (r_size_t i = 0; i < n; ++i) {
    sexp* elt = r_list_get(dots, i);
    sexp* expr = dot_get_expr(elt);
    sexp* env = dot_get_env(elt);

    // Unquoting rearranges expressions
    expr = KEEP(r_duplicate(expr, false));

    if (capture_info->unquote_names && r_is_call(expr, ":=")) {
      sexp* name = def_unquote_name(expr, env);

      if (dots_names == r_null) {
        dots_names = KEEP(r_new_vector(r_type_character, n));
        r_push_names(dots, dots_names);
        FREE(1);
      }

      if (r_chr_has_empty_string_at(dots_names, i)) {
        r_chr_poke(dots_names, i, name);
      } else {
        r_abort("Can't supply both `=` and `:=`");
      }
      expr = r_node_cadr(r_node_cdr(expr));
    }

    struct expansion_info info = which_expansion_op(expr);
    enum dots_expansion_op dots_op = info.op + (N_EXPANSION_OPS * capture_info->type);

    // Ignore empty arguments
    if (expr == r_missing_sym
        && (dots_names == r_null || r_chr_has_empty_string_at(dots_names, i))
        && should_ignore(capture_info->ignore_empty, i, n)) {
      mark_ignored_dot(elt);
      FREE(1);
      continue;
    }

    switch (dots_op) {
    case OP_EXPR_NONE:
    case OP_EXPR_UQ:
    case OP_EXPR_UQE:
      expr = call_interp_impl(expr, env, info);
      capture_info->count += 1;
      break;
    case OP_EXPR_UQS:
      mark_spliced_dots(elt);
      expr = dots_big_bang(capture_info, info.operand, env, false);
      break;
    case OP_QUO_NONE:
    case OP_QUO_UQ:
    case OP_QUO_UQE: {
      expr = KEEP(call_interp_impl(expr, env, info));
      expr = forward_quosure(expr, env);
      FREE(1);
      capture_info->count += 1;
      break;
    }
    case OP_QUO_UQS: {
      mark_spliced_dots(elt);
      expr = dots_big_bang(capture_info, info.operand, env, true);
      break;
    }
    case OP_VALUE_NONE:
      if (expr == r_missing_sym) {
        r_abort("Argument %d is empty", i + 1);
      }
      expr = r_eval(expr, env);
      capture_info->count += 1;
      break;
    case OP_VALUE_UQ:
    case OP_VALUE_UQE:
      r_abort("Can't use `!!` in a non-quoting function");
    case OP_VALUE_UQS: {
      expr = KEEP(r_eval(info.operand, env));
      if (expr == r_null) {
        mark_ignored_dot(elt);
        FREE(2);
        continue;
      }
      expr = set_value_spliced(expr);
      FREE(1);
      capture_info->count += 1;
      break;
    }}

    dot_poke_expr(elt, expr);
    FREE(1);
  }

  return dots;
}


static int match_ignore_empty_arg(sexp* ignore_empty) {
  if (!r_is_character(ignore_empty) || r_length(ignore_empty) == 0) {
    r_abort("`.ignore_empty` must be a character vector");
  }
  const char* arg = r_c_string(ignore_empty);
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
    if (r_is_integerish(named)) {
      return REAL(named)[0];
    }
    // else fallthrough
  default:
    break;
  }

 error:
  r_abort("`.named` must be a scalar logical or number");
}

sexp* capturedots(sexp* frame);

sexp* dots_interp(enum dots_capture_type type, sexp* frame_env,
                  sexp* named, sexp* ignore_empty, sexp* unquote_names) {
  if (!rlang_spliced_flag) rlang_spliced_flag = r_sym("__rlang_spliced");
  if (!rlang_ignored_flag) rlang_ignored_flag = r_sym("__rlang_ignored");

  sexp* dots = KEEP(capturedots(frame_env));
  struct dots_capture_info capture_info = init_capture_info(type, ignore_empty, unquote_names);
  dots = dots_unquote(dots, &capture_info);

  sexp* out = KEEP(r_new_vector(r_type_list, capture_info.count));

  // Dots captured by values don't have default empty names
  sexp* dots_info_names = r_names(dots);
  sexp* out_names = NULL;
  if (type != DOTS_VALUE || dots_info_names != r_null) {
    out_names = KEEP(r_new_vector(r_type_character, capture_info.count));
    r_push_names(out, out_names);
    FREE(1);
  }

  for (size_t i = 0, count = 0; i < r_length(dots); ++i) {
    sexp* elt = r_list_get(dots, i);
    sexp* expr = dot_get_expr(elt);

    if (is_ignored_dot(elt)) {
      continue;
    }

    if (is_spliced_dots(elt)) {
      // FIXME: Should be able to avoid conversion to pairlist for
      // lists, node lists, and character vectors
      while (expr != r_null) {
        sexp* head = r_node_car(expr);
        r_list_poke(out, count, head);

        sexp* tag = r_node_tag(expr);
        if (tag == r_null) {
          tag = r_string("");
        } else {
          tag = r_sym_str(tag);
          // Serialised unicode points might arise when unquoting
          // lists because of the conversion to pairlist
          tag = r_str_unserialise_unicode(tag);
        }
        if (out_names) {
          r_chr_poke(out_names, count, tag);
        }

        ++count;
        expr = r_node_cdr(expr);
      }
    } else {
      r_list_poke(out, count, expr);
      if (out_names && dots_info_names != r_null) {
        sexp* name = r_chr_get(dots_info_names, i);
        r_chr_poke(out_names, count, name);
      }
      ++count;
    }
  }

  int names_width = find_auto_names_width(named);
  if (names_width && (!out_names || r_chr_has(out_names, ""))) {
    sexp* auto_fn = rlang_ns_get("quos_auto_name");
    sexp* width = KEEP(r_scalar_int(names_width));
    sexp* auto_call = KEEP(r_build_call2(auto_fn, out, width));
    out = r_eval(auto_call, r_empty_env);
    FREE(2);
  }

  FREE(2);
  return out;
}

sexp* rlang_exprs_interp(sexp* frame_env, sexp* named,
                         sexp* ignore_empty, sexp* unquote_names) {
  sexp* dots = dots_interp(DOTS_EXPR, frame_env, named, ignore_empty, unquote_names);

  if (dots == r_null) {
    return empty_named_list();
  } else {
    return dots;
  }
}
sexp* rlang_quos_interp(sexp* frame_env, sexp* named,
                        sexp* ignore_empty, sexp* unquote_names) {
  sexp* dots = dots_interp(DOTS_QUO, frame_env, named, ignore_empty, unquote_names);

  if (dots == r_null) {
    return empty_quosures();
  } else {
    KEEP(dots);
    r_push_class(dots, "quosures");
    FREE(1);
    return dots;
  }
}
sexp* rlang_dots_interp(sexp* frame_env, sexp* named, sexp* ignore_empty) {
  sexp* unquote_names = KEEP(r_scalar_lgl(1));
  sexp* dots = dots_interp(DOTS_VALUE, frame_env, named, ignore_empty, unquote_names);
  FREE(1);

  if (dots == r_null) {
    return r_new_vector(r_type_list, 0);
  } else {
    return dots;
  }
}

sexp* rlang_dots_list(sexp* frame_env, sexp* named, sexp* ignore_empty) {
  sexp* dots = KEEP(rlang_dots_interp(frame_env, named, ignore_empty));
  dots = r_squash_if(dots, r_type_list, &r_is_spliced, 1);
  FREE(1);
  return dots;
}
sexp* rlang_dots_flat_list(sexp* frame_env, sexp* named, sexp* ignore_empty) {
  sexp* dots = KEEP(rlang_dots_interp(frame_env, named, ignore_empty));
  dots = r_squash_if(dots, r_type_list, &r_is_spliced_bare, 1);
  FREE(1);
  return dots;
}
