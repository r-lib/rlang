#include "rlang/rlang.h"
#include "expr-interp.h"

SEXP rlang_ns_get(const char* name);


SEXP rlang_capture_dots(SEXP frame_env) {
  static SEXP capture_call = NULL;

  if (!capture_call) {
    capture_call = KEEP(r_new_call_node(rlang_ns_get("captureDots"), r_null));
    r_preserve(capture_call);
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

static SEXP named_empty_list() {
  static SEXP empty_list = NULL;
  if (!empty_list) {
    empty_list = r_new_vector(VECSXP, 0);
    r_preserve(empty_list);
    r_mark_shared(empty_list);

    SEXP nms = KEEP(r_new_vector(STRSXP, 0));
    r_poke_names(empty_list, nms);
    FREE(1);
  }

  return empty_list;
}

static SEXP def_unquote_name(SEXP expr, SEXP env) {
  SEXP lhs = r_node_cadr(expr);

  SEXP bang_expr;
  int level = bang_level(lhs, &bang_expr);
  if (level == 3) {
    r_abort("The LHS of `:=` can't be spliced with `!!!`");
  } else if (level == 2) {
    lhs = r_eval(bang_expr, env);
  }

  int err = 0;
  lhs = r_new_symbol(lhs, &err);
  if (err) {
    r_abort("The LHS of `:=` must be a string or a symbol");
  }

  return r_sym_str(lhs);
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

static inline bool was_spliced(SEXP x) {
  return r_get_attribute(x, rlang_spliced_flag) == r_null;
}
static inline void mark_spliced(SEXP x) {
  r_poke_attribute(x, rlang_spliced_flag, rlang_spliced_flag);
}

static SEXP quo_uqs_coerce(SEXP expr) {
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
static SEXP quo_uqs(SEXP expr, SEXP env, r_size_t* count) {
  SEXP spliced_node = KEEP(r_eval(expr, env));
  spliced_node = quo_uqs_coerce(spliced_node);

  SEXP node = spliced_node;
  while (node != r_null) {
    expr = r_node_car(node);
    r_node_poke_car(node, rlang_forward_quosure(expr, env));

    node = r_node_cdr(node);
    *count += 1;
  }

  FREE(1);
  return spliced_node;
}

static SEXP dots_unquote(SEXP dots, r_size_t* count, SEXP op_offset) {
  *count = 0;

  for (r_size_t i = 0; i < r_length(dots); ++i) {
    SEXP elt = r_list_get(dots, i);
    SEXP expr = dot_get_expr(elt);
    SEXP env = dot_get_env(elt);

    if (r_is_call(expr, ":=")) {
      SEXP dots_names = r_names(dots);
      SEXP name = def_unquote_name(expr, env);

      // Unserialise unicode points such as <U+xxx> that arise when
      // UTF-8 names are converted to symbols and the native encoding
      // does not support the characters (i.e. all the time on Windows)
      name = r_str_unserialise_unicode(name);

      if (r_chr_has_empty_string_at(dots_names, i)) {
        r_chr_poke(dots_names, i, name);
      } else {
        r_abort("Can't supply both `=` and `:=`");
      }
      expr = r_node_cadr(r_node_cdr(expr));
    }

    SEXP operand;
    int offset = r_c_int(op_offset);
    enum expansion_op op = which_expand_op(expr, &operand) + offset;

    switch (op) {
    case OP_EXPR_NONE:
    case OP_EXPR_UQ:
    case OP_EXPR_UQS:
      r_abort("TODO EXPR %d", op);
    case OP_QUO_NONE:
      expr = interp_lang(expr, env);
      expr = rlang_forward_quosure(expr, env);
      *count += 1;
      break;
    case OP_QUO_UQ: {
      SEXP unquoted = KEEP(r_eval(operand, env));
      expr = rlang_forward_quosure(unquoted, env);
      FREE(1);
      *count += 1;
      break;
    }
    case OP_QUO_UQS: {
      mark_spliced(elt);
      expr = quo_uqs(operand, env, count);
      break;
    }
    case OP_VALUE_NONE:
    case OP_VALUE_UQ:
    case OP_VALUE_UQS:
      r_abort("TODO VALUE %d", op);
    }

    dot_poke_expr(elt, expr);
  }

  return dots;
}

SEXP rlang_dots_interp(SEXP frame_env, SEXP offset) {
  if (!rlang_spliced_flag) {
    rlang_spliced_flag = r_sym("__rlang_spliced");
  }

  r_size_t total;
  SEXP dots_info = KEEP(rlang_capture_dots(frame_env));
  SEXP dots_info_names = r_names(dots_info);

  dots_info = dots_unquote(dots_info, &total, offset);

  if (total == 0) {
    FREE(1);
    return named_empty_list();
  }

  SEXP dots = KEEP(r_new_vector(VECSXP, total));
  SEXP dots_names = KEEP(r_new_vector(STRSXP, total));
  r_push_names(dots, dots_names);

  for (size_t i = 0, count = 0; i < r_length(dots_info); ++i) {
    SEXP elt = r_list_get(dots_info, i);
    SEXP expr = dot_get_expr(elt);

    if (was_spliced(elt)) {
      r_list_poke(dots, count, expr);
      SEXP name = r_chr_get(dots_info_names, i);
      r_chr_poke(dots_names, count, name);
      ++count;
    } else {
      // FIXME: Should be able to avoid conversion to pairlist and use
      // a generic vec_get() or coll_get to walk the new elements
      while (expr != r_null) {
        SEXP head = r_node_car(expr);
        r_list_poke(dots, count, head);

        SEXP tag = r_node_tag(expr);
        if (tag == r_null) {
          tag = r_string("");
        } else {
          tag = r_sym_str(tag);
          // Serialised unicode points might arise when unquoting
          // lists because of the conversion to pairlist
          tag = r_str_unserialise_unicode(tag);
        }
        r_chr_poke(dots_names, count, tag);

        ++count;
        expr = r_node_cdr(expr);
      }
    }
  }

  FREE(3);
  return dots;
}
