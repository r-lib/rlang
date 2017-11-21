#include "rlang/rlang.h"
#include "expr-interp.h"

SEXP rlang_ns_get(const char* name);


SEXP rlang_capture_dots(SEXP frame_env) {
  static SEXP capture_call = NULL;

  if (!capture_call) {
    capture_call = KEEP(r_new_call_node(rlang_ns_get("captureDots"), r_null));
    r_mark_precious(capture_call);
    r_mark_shared(capture_call);
    FREE(1);
  }

  return r_eval(capture_call, frame_env);
}

// FIXME: is it right to disregard defs?
SEXP rlang_forward_quosure(SEXP x, SEXP env) {
  if (r_is_quosure(x)) {
    return x;
  } else if (r_is_symbolic(x)) {
    return r_new_quosure(x, env);
  } else {
    return r_new_quosure(x, r_empty_env);
  }
}

static inline SEXP dot_get_expr(SEXP dot) {
  return r_list_get(dot, 0);
}
static inline SEXP dot_get_env(SEXP dot) {
  return r_list_get(dot, 1);
}
static inline void dot_poke_expr(SEXP dot, SEXP elt) {
  r_list_poke(dot, 0, elt);
}

SEXP rlang_capture_dots(SEXP frame_env);

static SEXP new_preserved_empty_list() {
  SEXP empty_list = r_new_vector(VECSXP, 0);
  r_mark_precious(empty_list);
  r_mark_shared(empty_list);

  SEXP nms = KEEP(r_new_vector(STRSXP, 0));
  r_poke_names(empty_list, nms);
  FREE(1);

  return empty_list;
}
static SEXP empty_named_list() {
  static SEXP empty_list = NULL;
  if (!empty_list) {
    empty_list = new_preserved_empty_list();
  }
  return empty_list;
}
static SEXP empty_quosures() {
  static SEXP empty_quos = NULL;
  if (!empty_quos) {
    empty_quos = new_preserved_empty_list();
    r_push_class(empty_quos, "quosures");
  }
  return empty_quos;
}

static SEXP def_unquote_name(SEXP expr, SEXP env) {
  int n_kept = 0;
  SEXP lhs = r_node_cadr(expr);

  SEXP operand;
  int level = bang_level(lhs, &operand);
  switch (level) {
  case 2:
    lhs = KEEP(r_eval(operand, env));
    ++n_kept;
    break;
  case 3:
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
  }

  int err = 0;
  lhs = r_new_symbol(lhs, &err);
  if (err) {
    r_abort("The LHS of `:=` must be a string or a symbol");
  }

  SEXP name = r_sym_str(lhs);

  // Unserialise unicode points such as <U+xxx> that arise when
  // UTF-8 names are converted to symbols and the native encoding
  // does not support the characters (i.e. all the time on Windows)
  name = r_str_unserialise_unicode(name);

  FREE(n_kept);
  return name;
}


enum expansion_op {
  OP_EXPR_NONE,
  OP_EXPR_UQ,
  OP_EXPR_UQS,
  OP_QUO_NONE,
  OP_QUO_UQ,
  OP_QUO_UQS,
  OP_VALUE_NONE,
  OP_VALUE_UQ,
  OP_VALUE_UQS
};

static SEXP rlang_spliced_flag = NULL;
static SEXP rlang_ignored_flag = NULL;

static inline bool is_spliced_dots(SEXP x) {
  return r_get_attribute(x, rlang_spliced_flag) != r_null;
}
static inline void mark_spliced_dots(SEXP x) {
  r_poke_attribute(x, rlang_spliced_flag, rlang_spliced_flag);
}
static inline bool is_ignored_dot(SEXP x) {
  return r_get_attribute(x, rlang_ignored_flag) != r_null;
}
static inline void mark_ignored_dot(SEXP x) {
  r_poke_attribute(x, rlang_ignored_flag, rlang_ignored_flag);
}

static SEXP uqs_coerce(SEXP expr) {
  switch (r_kind(expr)) {
  case NILSXP:
  case LISTSXP:
    return expr;
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP: {
    static SEXP coercer = NULL;
    if (!coercer) { coercer = r_base_ns_get("as.pairlist"); }
    SEXP coerce_args = KEEP(r_new_node(expr, r_null));
    SEXP coerce_call = KEEP(r_new_call_node(coercer, coerce_args));
    SEXP coerced = r_eval(coerce_call, r_empty_env);
    FREE(2);
    return coerced;
  }
  case LANGSXP:
    if (r_is_symbol(r_node_car(expr), "{")) {
      return r_node_cdr(expr);
    }
    // else fallthrough
  default:
    return r_new_node(expr, r_null);
  }
}
static SEXP uqs(SEXP expr, SEXP env, r_size_t* count, bool quosured) {
  SEXP spliced_node = KEEP(r_eval(expr, env));
  spliced_node = uqs_coerce(spliced_node);

  SEXP node = spliced_node;
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

static SEXP dots_unquote(SEXP dots, r_size_t* count,
                         int op_offset, int ignore_empty) {
  SEXP dots_names = r_names(dots);
  *count = 0;
  r_size_t n = r_length(dots);

  for (r_size_t i = 0; i < n; ++i) {
    SEXP elt = r_list_get(dots, i);
    SEXP expr = dot_get_expr(elt);
    SEXP env = dot_get_env(elt);

    // Unquoting rearranges expressions
    expr = KEEP(r_duplicate(expr, false));

    if (r_is_call(expr, ":=")) {
      SEXP name = def_unquote_name(expr, env);

      if (dots_names == r_null || r_chr_has_empty_string_at(dots_names, i)) {
        r_chr_poke(dots_names, i, name);
      } else {
        r_abort("Can't supply both `=` and `:=`");
      }
      expr = r_node_cadr(r_node_cdr(expr));
    }

    SEXP operand;
    int base_op = which_expand_op(expr, &operand);
    enum expansion_op op = base_op + op_offset;

    // Ignore empty arguments
    if (expr == r_missing_sym
        && base_op == 0
        && (dots_names == r_null || r_chr_has_empty_string_at(dots_names, i))
        && should_ignore(ignore_empty, i, n)) {
      mark_ignored_dot(elt);
      FREE(1);
      continue;
    }

    switch (op) {
    case OP_EXPR_NONE:
    case OP_EXPR_UQ:
      expr = interp_lang(expr, env);
      *count += 1;
      break;
    case OP_EXPR_UQS:
      mark_spliced_dots(elt);
      expr = uqs(operand, env, count, false);
      break;
    case OP_QUO_NONE:
      expr = interp_lang(expr, env);
      expr = rlang_forward_quosure(expr, env);
      *count += 1;
      break;
    case OP_QUO_UQ: {
      expr = KEEP(interp_lang(expr, env));
      expr = rlang_forward_quosure(expr, env);
      FREE(1);
      *count += 1;
      break;
    }
    case OP_QUO_UQS: {
      mark_spliced_dots(elt);
      expr = uqs(operand, env, count, true);
      break;
    }
    case OP_VALUE_NONE:
    case OP_VALUE_UQ:
    case OP_VALUE_UQS:
      r_abort("TODO VALUE %d", op);
    }

    dot_poke_expr(elt, expr);
    FREE(1);
  }

  return dots;
}

static int match_ignore_empty_arg(SEXP ignore_empty) {
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

static int find_auto_names_width(SEXP named) {
  if (r_length(named) != 1) {
    goto error;
  }

  switch (r_kind(named)) {
  case LGLSXP:
    if (r_as_bool(named)) {
      return 60;
    } else {
      return 0;
    }
  case INTSXP:
    return INTEGER(named)[0];
  case REALSXP:
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


SEXP dots_interp(SEXP frame_env, SEXP named, SEXP ignore_empty, int offset) {
  if (!rlang_spliced_flag) {
    rlang_spliced_flag = r_sym("__rlang_spliced");
  }
  if (!rlang_ignored_flag) {
    rlang_ignored_flag = r_sym("__rlang_ignored");
  }

  SEXP dots_info = KEEP(rlang_capture_dots(frame_env));
  SEXP dots_info_names = r_names(dots_info);

  r_size_t total;
  int ignore_empty_int = match_ignore_empty_arg(ignore_empty);
  dots_info = dots_unquote(dots_info, &total, offset, ignore_empty_int);

  SEXP out = KEEP(r_new_vector(VECSXP, total));

  // Dots captured by values don't have default empty names
  SEXP out_names = NULL;
  if (offset != 6 || dots_info_names != r_null) {
    out_names = KEEP(r_new_vector(STRSXP, total));
    r_push_names(out, out_names);
    FREE(1);
  }

  for (size_t i = 0, count = 0; i < r_length(dots_info); ++i) {
    SEXP elt = r_list_get(dots_info, i);
    SEXP expr = dot_get_expr(elt);

    if (is_ignored_dot(elt)) {
      continue;
    }

    if (is_spliced_dots(elt)) {
      // FIXME: Should be able to avoid conversion to pairlist and use
      // a generic vec_get() or coll_get to walk the new elements
      while (expr != r_null) {
        SEXP head = r_node_car(expr);
        r_list_poke(out, count, head);

        SEXP tag = r_node_tag(expr);
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
        SEXP name = r_chr_get(dots_info_names, i);
        r_chr_poke(out_names, count, name);
      }
      ++count;
    }
  }

  int names_width = find_auto_names_width(named);
  if (names_width) {
    SEXP auto_fn = rlang_ns_get("quos_auto_name");
    SEXP width = KEEP(r_scalar_int(names_width));
    SEXP auto_call = KEEP(r_build_call2(auto_fn, out, width));
    out = r_eval(auto_call, r_empty_env);
    FREE(2);
  }

  FREE(2);
  return out;
}

SEXP rlang_exprs_interp(SEXP frame_env, SEXP named, SEXP ignore_empty) {
  SEXP dots = dots_interp(frame_env, named, ignore_empty, 0);

  if (dots == r_null) {
    return empty_named_list();
  } else {
    return dots;
  }
}
SEXP rlang_quos_interp(SEXP frame_env, SEXP named, SEXP ignore_empty) {
  SEXP dots = dots_interp(frame_env, named, ignore_empty, 3);

  if (dots == r_null) {
    return empty_quosures();
  } else {
    KEEP(dots);
    r_push_class(dots, "quosures");
    FREE(1);
    return dots;
  }
}
