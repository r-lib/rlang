#include "rlang/rlang.h"

SEXP rlang_ns_get(const char* name);

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


static int bang_level(SEXP x) {
  if (!r_is_call(x, "!")) {
    return 0;
  }

  SEXP arg = r_node_cdr(x);
  if (r_is_null(arg) || !r_is_call(r_node_car(arg), "!")) {
    return 1;
  }

  arg = r_node_cdr(r_node_car(arg));
  if (r_is_null(arg) || !r_is_call(r_node_car(arg), "!")) {
    return 2;
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
  int bang = bang_level(x);
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
    if (bang_level(next_head) == 3) {
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

static inline SEXP dot_expr(SEXP dot) {
  return r_list_get(dot, 0);
}
static inline SEXP dot_env(SEXP dot) {
  return r_list_get(dot, 1);
}

SEXP rlang_capture_dots(SEXP frame_env);

SEXP rlang_dots_quos(SEXP frame_env) {
  SEXP dots = KEEP(rlang_capture_dots(frame_env));

  SEXP node = dots;
  SEXP parent = r_null;

  while (node != r_null) {
    SEXP expr = dot_expr(r_node_car(node));
    SEXP env = dot_env(r_node_car(node));

    if (bang_level(expr) == 3) {
      expr = r_duplicate(expr, false);
      expr = replace_triple_bang(expr);
      r_node_poke_car(node, expr);
    }
    if (is_splice_call(expr)) {
      SEXP spliced = r_eval(expr, env);

      if (r_length(spliced)) {
        SEXP rest = r_node_cdr(node);
        r_node_poke_car(node, r_node_car(spliced));
        r_node_poke_cdr(node, r_node_cdr(spliced));

        SEXP tail;
        while (node != r_null) {
          expr = r_node_car(node);
          r_node_poke_car(node, rlang_forward_quosure(expr, env));
          tail = node;
          node = r_node_cdr(node);
        }

        r_node_poke_cdr(tail, rest);
        node = tail;
      } else if (parent == r_null) {
        dots = node = r_node_cdr(node);
        continue; // Don't poke `parent`
      } else {
        r_node_poke_cdr(parent, r_node_cdr(node));
      }

    } else {
      r_node_poke_car(node, rlang_forward_quosure(expr, env));
    }

    parent = node;
    node = r_node_cdr(node);
  }

  if (dots == r_null) {
    static SEXP empty_list = NULL;
    if (!empty_list) {
      empty_list = r_new_vector(VECSXP, 0);
      r_preserve(empty_list);
      r_mark_shared(empty_list);

      SEXP nms = KEEP(r_new_vector(STRSXP, 0));
      r_poke_names(empty_list, nms);
      FREE(1);
    }
    dots = empty_list;
  }

  FREE(1);
  return dots;
}
