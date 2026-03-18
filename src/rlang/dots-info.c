#include <rlang.h>

#define RLANG_HAS_R_DOTS_API (R_VERSION >= R_Version(4, 6, 0))

#if !RLANG_HAS_R_DOTS_API

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

static
r_obj* env_dot_find(r_obj* env, r_ssize i) {
    if (i < 0) {
        r_abort("indexing '...' with negative index %d", (int) i);
    }

    r_obj* dots = Rf_findVarInFrame(env, r_syms.dots);

    if (dots == R_UnboundValue) {
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

#endif


// Dots API - mirrors R-devel PR #209 ---
// See https://github.com/r-devel/r-svn/pull/209


// R API: R_DotsExist
bool r_env_dots_exist(r_obj* env) {
#if RLANG_HAS_R_DOTS_API
    return R_DotsExist(env);
#else
    r_obj* dots = Rf_findVarInFrame(env, r_syms.dots);
    return dots != R_UnboundValue && (dots == r_syms.missing || r_typeof(dots) == R_TYPE_dots);
#endif
}

r_obj* r_env_until_dots(r_obj* env) {
    while (env != r_envs.empty) {
        if (r_env_dots_exist(env)) {
            return env;
        }
        env = r_env_parent(env);
    }
    return r_envs.empty;
}

// R API: R_DotsLength
r_ssize r_env_dots_length(r_obj* env) {
#if RLANG_HAS_R_DOTS_API
    return (r_ssize) R_DotsLength(env);
#else
    r_obj* dots = Rf_findVarInFrame(env, r_syms.dots);

    if (dots == R_UnboundValue) {
        r_abort("incorrect context: the current call has no '...' to look in");
    }

    if (dots == r_syms.missing || r_typeof(dots) != R_TYPE_dots) {
        return 0;
    }

    return r_length(dots);
#endif
}

// R API: R_DotsNames
// Returns NULL when all dots are unnamed.
r_obj* r_env_dots_names(r_obj* env) {
#if RLANG_HAS_R_DOTS_API
    return R_DotsNames(env);
#else
    r_obj* dots = KEEP(Rf_findVarInFrame(env, r_syms.dots));

    if (dots == R_UnboundValue) {
        r_abort("incorrect context: the current call has no '...' to look in");
    }

    r_ssize n = (dots == r_syms.missing || r_typeof(dots) != R_TYPE_dots) ? 0 : r_length(dots);

    r_obj* out = r_null;

    for (r_ssize i = 0; i < n; ++i) {
        r_obj* tag = r_node_tag(dots);
        if (r_typeof(tag) == R_TYPE_symbol) {
            if (out == r_null) {
                out = KEEP(r_alloc_character(n));
            }
            r_chr_poke(out, i, r_sym_string(tag));
        }
        dots = r_node_cdr(dots);
    }

    if (out != r_null) {
        FREE(1);
    }

    FREE(1);
    return out;
#endif
}

// R API: R_DotsElt
r_obj* r_env_dot_get(r_obj* env, r_ssize i) {
    if (r_env_dot_type(env, i) == DOT_TYPE_missing) {
        return r_missing_arg;
    }

#if RLANG_HAS_R_DOTS_API
    return R_DotsElt((int)(i + 1), env);
#else
    r_obj* elt = env_dot_find(env, i);
    return r_eval(elt, env);
#endif
}

// R API: R_GetDotType
r_dot_type_t r_env_dot_type(r_obj* env, r_ssize i) {
#if RLANG_HAS_R_DOTS_API
    return (r_dot_type_t) R_GetDotType((int)(i + 1), env);
#else
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
#endif
}

// R API: R_DotDelayedExpression
r_obj* r_env_dot_delayed_expr(r_obj* env, r_ssize i) {
#if RLANG_HAS_R_DOTS_API
    return R_DotDelayedExpression((int)(i + 1), env);
#else
    r_obj* elt = env_dot_find(env, i);

    if (!is_promise(elt)) {
        r_abort("not a delayed ... element");
    }

    bool forced;
    r_obj* inner = rlang_promise_unwrap(elt, &forced);
    if (forced) {
        r_abort("not a delayed ... element");
    }

    return promise_expr(inner);
#endif
}

// R API: R_DotDelayedEnvironment
r_obj* r_env_dot_delayed_env(r_obj* env, r_ssize i) {
#if RLANG_HAS_R_DOTS_API
    return R_DotDelayedEnvironment((int)(i + 1), env);
#else
    r_obj* elt = env_dot_find(env, i);

    if (!is_promise(elt)) {
        r_abort("not a delayed ... element");
    }

    bool forced;
    r_obj* inner = rlang_promise_unwrap(elt, &forced);
    if (forced) {
        r_abort("not a delayed ... element");
    }

    return promise_env(inner);
#endif
}

// R API: R_DotForcedExpression
r_obj* r_env_dot_forced_expr(r_obj* env, r_ssize i) {
#if RLANG_HAS_R_DOTS_API
    return R_DotForcedExpression((int)(i + 1), env);
#else
    r_obj* elt = env_dot_find(env, i);

    if (!is_promise(elt)) {
        r_abort("not a forced ... element");
    }

    bool forced;
    r_obj* inner = rlang_promise_unwrap(elt, &forced);
    if (!forced) {
        r_abort("not a forced ... element");
    }

    return promise_expr(inner);
#endif
}
