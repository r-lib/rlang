#include "rlang/rlang.h"
#include "expr-interp.h"

sexp* rlang_ns_get(const char* name);


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


enum root_expansion_op {
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

static sexp* root_big_bang(sexp* expr, sexp* env, r_size_t* count, bool quosured) {
  sexp* spliced_node = KEEP(r_eval(expr, env));
  spliced_node = big_bang_coerce(spliced_node);

  sexp* node = spliced_node;
  while (node != r_null) {
    expr = r_node_car(node);
    if (quosured) {
      expr = rlang_forward_quosure(expr, env);
    }
    r_node_poke_car(node, expr);

    node = r_node_cdr(node);
    *count += 1;
  }

  FREE(1);
  return spliced_node;
}

static inline bool should_ignore(int ignore_empty, r_size_t i, r_size_t n) {
  return ignore_empty == 1 || (i == n - 1 && ignore_empty == -1);
}

static sexp* set_spliced(sexp* x) {
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

static sexp* dots_unquote(sexp* dots, r_size_t* count,
                         int op_offset, int ignore_empty) {
  sexp* dots_names = r_names(dots);
  *count = 0;
  r_size_t n = r_length(dots);

  for (r_size_t i = 0; i < n; ++i) {
    sexp* elt = r_list_get(dots, i);
    sexp* expr = dot_get_expr(elt);
    sexp* env = dot_get_env(elt);

    // Unquoting rearranges expressions
    expr = KEEP(r_duplicate(expr, false));

    if (r_is_call(expr, ":=")) {
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
    enum root_expansion_op root_op = info.op + op_offset;

    // Ignore empty arguments
    if (expr == r_missing_sym
        && (dots_names == r_null || r_chr_has_empty_string_at(dots_names, i))
        && should_ignore(ignore_empty, i, n)) {
      mark_ignored_dot(elt);
      FREE(1);
      continue;
    }

    switch (root_op) {
    case OP_EXPR_NONE:
    case OP_EXPR_UQ:
    case OP_EXPR_UQE:
      expr = call_interp_impl(expr, env, info);
      *count += 1;
      break;
    case OP_EXPR_UQS:
      mark_spliced_dots(elt);
      expr = root_big_bang(info.operand, env, count, false);
      break;
    case OP_QUO_NONE:
    case OP_QUO_UQ:
    case OP_QUO_UQE: {
      expr = KEEP(call_interp_impl(expr, env, info));
      expr = rlang_forward_quosure(expr, env);
      FREE(1);
      *count += 1;
      break;
    }
    case OP_QUO_UQS: {
      mark_spliced_dots(elt);
      expr = root_big_bang(info.operand, env, count, true);
      break;
    }
    case OP_VALUE_NONE:
      expr = r_eval(expr, env);
      *count += 1;
      break;
    case OP_VALUE_UQ:
    case OP_VALUE_UQE:
      r_abort("Can't use `!!` in a non-quoting function");
    case OP_VALUE_UQS: {
      expr = KEEP(r_eval(info.operand, env));
      expr = set_spliced(expr);
      FREE(1);
      *count += 1;
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

sexp* dots_interp(sexp* frame_env, sexp* named, sexp* ignore_empty, int op_offset) {
  if (!rlang_spliced_flag) rlang_spliced_flag = r_sym("__rlang_spliced");
  if (!rlang_ignored_flag) rlang_ignored_flag = r_sym("__rlang_ignored");

  sexp* dots_info = KEEP(capturedots(frame_env));

  r_size_t total;
  int ignore_empty_int = match_ignore_empty_arg(ignore_empty);
  dots_info = dots_unquote(dots_info, &total, op_offset, ignore_empty_int);

  sexp* out = KEEP(r_new_vector(r_type_list, total));

  // Dots captured by values don't have default empty names
  sexp* dots_info_names = r_names(dots_info);
  sexp* out_names = NULL;
  if (op_offset != 8 || dots_info_names != r_null) {
    out_names = KEEP(r_new_vector(r_type_character, total));
    r_push_names(out, out_names);
    FREE(1);
  }

  for (size_t i = 0, count = 0; i < r_length(dots_info); ++i) {
    sexp* elt = r_list_get(dots_info, i);
    sexp* expr = dot_get_expr(elt);

    if (is_ignored_dot(elt)) {
      continue;
    }

    if (is_spliced_dots(elt)) {
      // FIXME: Should be able to avoid conversion to pairlist and use
      // a generic vec_get() or coll_get to walk the new elements
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

sexp* rlang_exprs_interp(sexp* frame_env, sexp* named, sexp* ignore_empty) {
  sexp* dots = dots_interp(frame_env, named, ignore_empty, 0);

  if (dots == r_null) {
    return empty_named_list();
  } else {
    return dots;
  }
}
sexp* rlang_quos_interp(sexp* frame_env, sexp* named, sexp* ignore_empty) {
  sexp* dots = dots_interp(frame_env, named, ignore_empty, 4);

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
  sexp* dots = dots_interp(frame_env, named, ignore_empty, 8);

  if (dots == r_null) {
    return r_new_vector(r_type_list, 0);
  } else {
    return dots;
  }
}
