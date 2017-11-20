#include "rlang/rlang.h"

SEXP rlang_ns_get(const char* name);
SEXP r_str_unserialise_unicode(SEXP);

static SEXP interp_lang(SEXP x, SEXP env);
static SEXP interp_lang_node(SEXP x, SEXP env);


static inline bool is_rlang_call_any(SEXP x, const char** names, int n) {
  return
    r_is_call_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}

#define UQ_N 3
#define UQS_N 2

static const char* uq_names[UQ_N] = { "UQ", "UQE", "!!" };
static const char* uqs_names[UQS_N] = { "UQS", "!!!"};


#define FIXUP_OPS_N 10
#define FIXUP_UNARY_OPS_N 2

static const char* fixup_ops_names[FIXUP_OPS_N] = {
  "<", ">", "<=", ">=", "==", "!=", "*", "/", ":", "^"
};
static const char* fixup_unary_ops_names[FIXUP_UNARY_OPS_N] = {
  "-", "+"
};

static SEXP uqs_fun;


static int bang_level(SEXP x, SEXP* operand) {
  if (!r_is_call(x, "!")) {
    return 0;
  }

  SEXP arg = r_node_cadr(x);
  if (!r_is_call(arg, "!")) {
    return 0;
  }

  arg = r_node_cadr(arg);
  if (!r_is_call(arg, "!")) {
    if (operand) {
      *operand = arg;
    }
    return 2;
  }

  if (operand) {
    *operand = r_node_cadr(arg);
  }
  return 3;
}

static inline bool needs_fixup(SEXP x) {
  if (r_is_call_any(x, fixup_ops_names, FIXUP_OPS_N)) {
    return true;
  }

  // Don't fixup unary operators
  if (r_is_call_any(x, fixup_unary_ops_names, FIXUP_UNARY_OPS_N)) {
    return r_node_cddr(x) != r_null;
  }

  if (r_is_special_op_call(x)) {
    return true;
  }

  return false;
}

static SEXP uq_call(SEXP x) {
  SEXP args = KEEP(r_new_node_list(x));
  SEXP call = r_new_call_node(r_sym("UQ"), args);
  FREE(1);
  return call;
}
static SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x, NULL);
  if (bang == 3 || r_is_maybe_prefixed_call_any(x, uqs_names, UQS_N)) {
    r_abort("Can't splice at top-level");
  }
  if (bang != 2) {
    return x;
  }

  SEXP cadr = r_node_cadr(x);
  SEXP cadr_cadr = r_node_cadr(r_node_cadr(x));

  if (!needs_fixup(cadr_cadr)) {
    x = cadr;
    r_node_poke_car(x, r_sym("UQ"));
    return x;
  }

  // We have an infix expression. Fix up AST so that `!!` binds tightly
  x = cadr_cadr;

  SEXP innermost = x;
  while (needs_fixup(r_node_cadr(innermost))) {
    innermost = r_node_cadr(innermost);
  }

  r_node_poke_cadr(innermost, uq_call(r_node_cadr(innermost)));
  return x;
}
static SEXP replace_triple_bang(SEXP x) {
  SEXP node = r_node_cadr(r_node_cadr(x));

  r_node_poke_car(node, r_sym("UQS"));

  return node;
}

static void unquote_check(SEXP x) {
  if (r_node_cdr(x) == r_null)
    r_abort("`UQ()` must be called with an argument");
}

static SEXP unquote(SEXP x, SEXP env, SEXP uq_sym) {
  if (r_is_symbol(uq_sym, "!!")) {
    uq_sym = r_sym("UQE");
  }

  // Inline unquote function before evaluation because even `::` might
  // not be available in interpolation environment.
  SEXP uq_fun = KEEP(r_env_get(r_ns_env("rlang"), uq_sym));
  uq_fun = KEEP(Rf_lang2(uq_fun, x));

  SEXP unquoted = KEEP(r_eval(uq_fun, env));

  FREE(3);
  return unquoted;
}

static SEXP unquote_prefixed_uq(SEXP x, SEXP env) {
  SEXP uq_sym = r_node_cadr(r_node_cdar(x));
  SEXP unquoted = KEEP(unquote(r_node_cadr(x), env, uq_sym));
  r_node_poke_cdr(r_node_cdar(x), r_new_node(unquoted, r_null));
  FREE(1);

  if (r_is_namespaced_call(x, "rlang")) {
    x = r_node_cadr(r_node_cdar(x));
  } else {
    x = r_node_car(x);
  }
  return x;
}

static SEXP splice_next(SEXP node, SEXP next, SEXP env) {
  r_node_poke_car(r_node_car(next), uqs_fun);

  // UQS() does error checking and returns a pair list
  SEXP spliced_node = KEEP(r_eval(r_node_car(next), env));

  if (spliced_node == r_null) {
    r_node_poke_cdr(node, r_node_cdr(next));
  } else {
    // Insert spliced_node into existing pairlist of args
    r_node_poke_cdr(r_node_tail(spliced_node), r_node_cdr(next));
    r_node_poke_cdr(node, spliced_node);
  }

  FREE(1);
  return next;
}

static SEXP interp_lang(SEXP x, SEXP env)  {
  if (!uqs_fun) {
    uqs_fun = rlang_ns_get("UQS");
  }
  if (r_kind(x) != LANGSXP) {
    return x;
  }

  KEEP(x);
  x = replace_double_bang(x);

  if (r_is_prefixed_call_any(x, uq_names, UQ_N)) {
    unquote_check(x);
    x = unquote_prefixed_uq(x, env);
  } else if (r_is_call_any(x, uq_names, UQ_N)) {
    unquote_check(x);
    SEXP uq_sym = r_node_car(x);
    x = unquote(r_node_cadr(x), env, uq_sym);
  } else {
    x = interp_lang_node(x, env);
  }

  FREE(1);
  return x;
}

static inline bool is_splice_call(SEXP node) {
  return is_rlang_call_any(node, uqs_names, UQS_N);
}

static SEXP interp_lang_node(SEXP x, SEXP env) {
  SEXP node, next, next_head;

  for (node = x; node != r_null; node = r_node_cdr(node)) {
    r_node_poke_car(node, interp_lang(r_node_car(node), env));

    next = r_node_cdr(node);
    next_head = r_node_car(next);

    // FIXME double check
    if (bang_level(next_head, NULL) == 3) {
      next_head = replace_triple_bang(next_head);
      r_node_poke_car(next, next_head);
    }
    if (is_splice_call(next_head)) {
      node = splice_next(node, next, env);
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env) {
  if (r_kind(x) != LANGSXP) {
    return x;
  }
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }

  x = KEEP(r_duplicate(x, false));
  x = interp_lang(x, env);

  FREE(1);
  return x;
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

#define OP_EXPAND_NONE 0
#define OP_EXPAND_UQ 1
#define OP_EXPAND_UQS 2

static int which_expand_op(SEXP x, SEXP* operand) {
  int level = bang_level(x, operand);
  if (level == 2) {
    return OP_EXPAND_UQ;
  } else if (level == 3) {
    return OP_EXPAND_UQS;
  }

  if (is_rlang_call_any(x, uq_names, UQ_N)) {
    if (operand) {
      *operand = r_node_cadr(x);
    }
    return OP_EXPAND_UQ;
  }

  if (is_splice_call(x)) {
    if (operand) {
      *operand = r_node_cadr(x);
    }
    return OP_EXPAND_UQS;
  }

  return OP_EXPAND_NONE;
}


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
