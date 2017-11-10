#include "rlang.h"

static
SEXP interp_lang(SEXP x, SEXP env, bool quosured);
static
SEXP interp_lang_node(SEXP x, SEXP env, bool quosured);

static inline
bool is_rlang_call_any(SEXP x, const char** names, int n) {
  return
    r_is_language_any(x, names, n) ||
    r_is_namespaced_call_any(x, "rlang", names, n);
}

#define UQ_N 3
#define UQS_N 2

static const char*
uq_names[UQ_N] = { "UQ", "UQE", "!!" };

static const char*
uqs_names[UQS_N] = { "UQS", "!!!"};


static
int bang_level(SEXP x) {
  if (!r_is_language(x, "!"))
    return 0;

  SEXP arg = r_node_cdr(x);
  if (r_is_null(arg) || !r_is_language(r_node_car(arg), "!"))
    return 1;

  arg = r_node_cdr(r_node_car(arg));
  if (r_is_null(arg) || !r_is_language(r_node_car(arg), "!"))
    return 2;

  return 3;
}

static
SEXP replace_double_bang(SEXP x) {
  int bang = bang_level(x);
  if (bang == 3 || r_is_maybe_prefixed_call_any(x, uqs_names, UQS_N)) {
    r_abort("Can't splice at top-level");
  } else if (bang == 2) {
    x = r_node_cadr(x);
    r_node_poke_car(x, r_sym("UQ"));
  }
  return x;
}
static
SEXP replace_triple_bang(SEXP x) {
  if (bang_level(r_node_car(x)) != 3) {
    return x;
  }

  SEXP node = r_node_cdar(r_node_cdar(x));

  r_node_poke_car(r_node_car(node), r_sym("UQS"));
  r_node_poke_cdr(node, r_node_cdr(x));

  return node;
}

static
void unquote_check(SEXP x) {
  if (r_node_cdr(x) == r_null)
    r_abort("`UQ()` must be called with an argument");
}

static
SEXP unquote(SEXP x, SEXP env, SEXP uq_sym, bool quosured) {
  if (r_is_symbol(uq_sym, "!!")) {
    uq_sym = r_sym("UQE");
  }

  // Inline unquote function before evaluation because even `::` might
  // not be available in interpolation environment.
  SEXP uq_fun = KEEP(r_env_get(r_ns_env("rlang"), uq_sym));
  uq_fun = KEEP(Rf_lang2(uq_fun, x));

  SEXP unquoted = KEEP(r_eval(uq_fun, env));

  if (!quosured && is_symbolic(unquoted)) {
    unquoted = Rf_lang2(r_sym("quote"), unquoted);
  }

  FREE(3);
  return unquoted;
}

static
SEXP unquote_prefixed_uq(SEXP x, SEXP env, bool quosured) {
  SEXP uq_sym = r_node_cadr(r_node_cdar(x));
  SEXP unquoted = KEEP(unquote(r_node_cadr(x), env, uq_sym, quosured));
  r_node_poke_cdr(r_node_cdar(x), r_new_node_(unquoted, r_null));
  FREE(1);

  if (r_is_namespaced_call(x, "rlang")) {
    x = r_node_cadr(r_node_cdar(x));
  } else {
    x = r_node_car(x);
  }
  return x;
}

static
SEXP splice_next(SEXP node, SEXP next, SEXP env) {
  static SEXP uqs_fun;
  if (!uqs_fun)
    uqs_fun = rlang_obj("UQS");
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

static
SEXP value_splice_next(SEXP cur, SEXP nxt, SEXP env) {
  r_node_poke_car(r_node_car(nxt), rlang_obj("splice"));
  r_node_poke_car(nxt, r_eval(r_node_car(nxt), env));
  return cur;
}

static
SEXP interp_lang(SEXP x, SEXP env, bool quosured)  {
  if (r_kind(x) != LANGSXP) {
    return x;
  }

  KEEP(x);
  x = replace_double_bang(x);

  if (r_is_prefixed_call_any(x, uq_names, UQ_N)) {
    unquote_check(x);
    x = unquote_prefixed_uq(x, env, quosured);
  } else if (r_is_maybe_prefixed_call_any(x, uq_names, UQ_N)) {
    unquote_check(x);
    SEXP uq_sym = r_node_car(x);
    x = unquote(r_node_cadr(x), env, uq_sym, quosured);
  } else {
    x = interp_lang_node(x, env, quosured);
  }

  FREE(1);
  return x;
}

static inline
bool is_splice_call(SEXP node) {
  return is_rlang_call_any(r_node_car(node), uqs_names, UQS_N);
}

static
SEXP interp_lang_node(SEXP x, SEXP env, bool quosured) {
  SEXP node, next;

  for (node = x; node != r_null; node = r_node_cdr(node)) {
    r_node_poke_car(node, interp_lang(r_node_car(node), env, quosured));

    next = r_node_cdr(node);
    next = replace_triple_bang(next);

    if (is_splice_call(next)) {
      if (quosured) {
        node = splice_next(node, next, env);
      } else {
        node = value_splice_next(node, next, env);
      }
    }
  }

  return x;
}

SEXP rlang_interp(SEXP x, SEXP env, SEXP quosured) {
  if (r_kind(x) != LANGSXP) {
    return x;
  }
  if (!r_is_environment(env)) {
    r_abort("`env` must be an environment");
  }

  x = KEEP(r_duplicate(x, false));
  x = interp_lang(x, env, r_as_bool(quosured));

  FREE(1);
  return x;
}
