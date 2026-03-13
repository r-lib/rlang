#include <rlang.h>

// Internal promise helpers (static, used only in this file) ---

static
bool is_promise(r_obj* x) {
    return r_typeof(x) == R_TYPE_promise;
}

static
r_obj* promise_expr(r_obj* x) {
    return PREXPR(x);
}

static
r_obj* promise_env(r_obj* x) {
    return PRENV(x);
}


// Dots API - mirrors R-devel PR #209 ---
// See https://github.com/r-devel/r-svn/pull/209


// R API: R_DotsExist
bool r_env_dots_exist(r_obj* env) {
    r_obj* dots = Rf_findVar(r_syms.dots, env);
    return dots != r_syms.unbound;
}

// R API: R_DotsLength
r_ssize r_env_dots_length(r_obj* env) {
    r_obj* dots = Rf_findVar(r_syms.dots, env);

    if (dots == r_syms.unbound) {
        r_abort("incorrect context: the current call has no '...' to look in");
    }

    if (dots == r_syms.missing || r_typeof(dots) != R_TYPE_dots) {
        return 0;
    }

    return r_length(dots);
}

// R API: R_DotsNames
r_obj* r_env_dots_names(r_obj* env) {
    r_obj* dots = KEEP(Rf_findVar(r_syms.dots, env));

    if (dots == r_syms.unbound) {
        r_abort("incorrect context: the current call has no '...' to look in");
    }

    r_ssize n = (dots == r_syms.missing || r_typeof(dots) != R_TYPE_dots) ? 0 : r_length(dots);

    r_obj* out = KEEP(r_alloc_character(n));

    for (r_ssize i = 0; i < n; ++i) {
        r_obj* tag = r_node_tag(dots);
        if (r_typeof(tag) == R_TYPE_symbol) {
            r_chr_poke(out, i, r_sym_string(tag));
        } else {
            r_chr_poke(out, i, r_strs.empty);
        }
        dots = r_node_cdr(dots);
    }

    FREE(2);
    return out;
}

static
r_obj* env_dot_find(r_obj* env, r_ssize i) {
    if (i < 0) {
        r_abort("indexing '...' with negative index %d", (int) i);
    }

    r_obj* dots = Rf_findVar(r_syms.dots, env);

    if (dots == r_syms.unbound) {
        r_abort("'...' used in an incorrect context");
    }

    if (dots == r_syms.missing || r_typeof(dots) != R_TYPE_dots) {
        r_abort("the ... list contains fewer than %d elements", (int) i + 1);
    }

    for (r_ssize j = 0; j < i; ++j) {
        dots = r_node_cdr(dots);
        if (dots == r_null) {
            r_abort("the ... list contains fewer than %d elements", (int) i + 1);
        }
    }

    return r_node_car(dots);
}

// R API: R_DotsElt
r_obj* r_env_dot_get(r_obj* env, r_ssize i) {
    r_obj* elt = env_dot_find(env, i);
    return r_eval(elt, env);
}

// R API: R_GetDotType
r_dot_type_t r_env_dot_type(r_obj* env, r_ssize i) {
    r_obj* elt = env_dot_find(env, i);

    if (elt == r_syms.missing) {
        return DOT_TYPE_missing;
    }

    if (!is_promise(elt)) {
        return DOT_TYPE_value;
    }

    bool forced;
    rlang_promise_unwrap(elt, &forced);
    if (forced) {
        return DOT_TYPE_forced;
    }

    return DOT_TYPE_delayed;
}

// R API: R_DotDelayedExpression
r_obj* r_env_dot_delayed_expr(r_obj* env, r_ssize i) {
    r_obj* elt = env_dot_find(env, i);

    if (!is_promise(elt)) {
        r_abort("not a delayed promise");
    }

    bool forced;
    r_obj* inner = rlang_promise_unwrap(elt, &forced);
    if (forced) {
        r_abort("not a delayed promise");
    }

    return promise_expr(inner);
}

// R API: R_DotDelayedEnvironment
r_obj* r_env_dot_delayed_env(r_obj* env, r_ssize i) {
    r_obj* elt = env_dot_find(env, i);

    if (!is_promise(elt)) {
        r_abort("not a delayed promise");
    }

    bool forced;
    r_obj* inner = rlang_promise_unwrap(elt, &forced);
    if (forced) {
        r_abort("not a delayed promise");
    }

    return promise_env(inner);
}

// R API: R_DotForcedExpression
r_obj* r_env_dot_forced_expr(r_obj* env, r_ssize i) {
    r_obj* elt = env_dot_find(env, i);

    if (!is_promise(elt)) {
        r_abort("not a forced promise");
    }

    bool forced;
    r_obj* inner = rlang_promise_unwrap(elt, &forced);
    if (!forced) {
        r_abort("not a forced promise");
    }

    return promise_expr(inner);
}
