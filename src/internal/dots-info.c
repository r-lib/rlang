#include "dots-info.h"

// Internal promise helpers (static, used only in this file) ---

static Rboolean is_promise(SEXP x) {
    return TYPEOF(x) == PROMSXP;
}

static SEXP promise_expr(SEXP x) {
    return PREXPR(x);
}

static SEXP promise_env(SEXP x) {
    return PRENV(x);
}

static Rboolean promise_is_forced(SEXP x) {
    return PRVALUE(x) != R_UnboundValue;
}

// Unwrap nested promises to the innermost one.
// Sets `*forced` to TRUE if the innermost promise is forced.
static SEXP promise_unwrap(SEXP elt, Rboolean *forced) {
    if (!is_promise(elt))
        Rf_error("internal: expected a promise");

    while (true) {
        if (promise_is_forced(elt)) {
            *forced = TRUE;
            return elt;
        }

        SEXP expr = promise_expr(elt);
        if (!is_promise(expr)) {
            *forced = FALSE;
            return elt;
        }

        elt = expr;
    }
}


// Dots API - mirrors R-devel PR #209 ---
// See https://github.com/r-devel/r-svn/pull/209


// R API: R_DotsExist
Rboolean r_env_dots_exist(SEXP env) {
    SEXP dots = Rf_findVar(R_DotsSymbol, env);
    return dots != R_UnboundValue;
}

// R API: R_DotsLength
int r_env_dots_length(SEXP env) {
    SEXP dots = Rf_findVar(R_DotsSymbol, env);

    if (dots == R_UnboundValue)
        return -1;

    if (dots == R_MissingArg || TYPEOF(dots) != DOTSXP)
        return 0;

    return Rf_length(dots);
}

// R API: R_DotsNames
SEXP r_env_dots_names(SEXP env) {
    SEXP dots = PROTECT(Rf_findVar(R_DotsSymbol, env));

    if (dots == R_UnboundValue)
        Rf_error("incorrect context: the current call has no '...' to look in");

    int n = (dots == R_MissingArg || TYPEOF(dots) != DOTSXP) ? 0 : Rf_length(dots);

    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

    for (int i = 0; i < n; ++i) {
        SEXP tag = TAG(dots);
        if (TYPEOF(tag) == SYMSXP) {
            SET_STRING_ELT(out, i, PRINTNAME(tag));
        } else {
            SET_STRING_ELT(out, i, R_BlankString);
        }
        dots = CDR(dots);
    }

    UNPROTECT(2);
    return out;
}

static
SEXP env_dot_find(SEXP env, r_ssize i) {
    if (i <= 0)
        Rf_error("indexing '...' with non-positive index %d", (int) i);

    SEXP dots = Rf_findVar(R_DotsSymbol, env);

    if (dots == R_UnboundValue)
        Rf_error("'...' used in an incorrect context");

    if (dots == R_MissingArg || TYPEOF(dots) != DOTSXP)
        Rf_error("the ... list contains fewer than %d elements", (int) i);

    for (r_ssize j = 1; j < i; ++j) {
        dots = CDR(dots);
        if (dots == R_NilValue)
            Rf_error("the ... list contains fewer than %d elements", (int) i);
    }

    return CAR(dots);
}

// R API: R_DotsElt
SEXP r_env_dot_get(SEXP env, r_ssize i) {
    SEXP elt = env_dot_find(env, i);
    return Rf_eval(elt, env);
}

// R API: R_GetDotType
r_dot_type_t r_env_dot_type(SEXP env, r_ssize i) {
    SEXP elt = env_dot_find(env, i);

    if (elt == R_MissingArg)
        return DOT_TYPE_missing;

    if (!is_promise(elt))
        return DOT_TYPE_value;

    Rboolean forced;
    promise_unwrap(elt, &forced);
    if (forced)
        return DOT_TYPE_forced;

    return DOT_TYPE_delayed;
}

// R API: R_DotDelayedExpression
SEXP r_env_dot_delayed_expr(SEXP env, r_ssize i) {
    SEXP elt = env_dot_find(env, i);

    if (!is_promise(elt))
        Rf_error("not a delayed promise");

    Rboolean forced;
    SEXP inner = promise_unwrap(elt, &forced);
    if (forced)
        Rf_error("not a delayed promise");

    return promise_expr(inner);
}

// R API: R_DotDelayedEnvironment
SEXP r_env_dot_delayed_env(SEXP env, r_ssize i) {
    SEXP elt = env_dot_find(env, i);

    if (!is_promise(elt))
        Rf_error("not a delayed promise");

    Rboolean forced;
    SEXP inner = promise_unwrap(elt, &forced);
    if (forced)
        Rf_error("not a delayed promise");

    return promise_env(inner);
}

// R API: R_DotForcedExpression
SEXP r_env_dot_forced_expr(SEXP env, r_ssize i) {
    SEXP elt = env_dot_find(env, i);

    if (!is_promise(elt))
        Rf_error("not a forced promise");

    Rboolean forced;
    SEXP inner = promise_unwrap(elt, &forced);
    if (!forced)
        Rf_error("not a forced promise");

    return promise_expr(inner);
}


// FFI wrappers for R interface ---

SEXP ffi_dots_exist(SEXP env) {
    return Rf_ScalarLogical(r_env_dots_exist(env));
}

SEXP ffi_dots_length(SEXP env) {
    int n = r_env_dots_length(env);

    if (n < 0) {
        Rf_error("incorrect context: the current call has no '...' to look in");
    }

    return Rf_ScalarInteger(n);
}

SEXP ffi_dots_names(SEXP env) {
    return r_env_dots_names(env);
}

SEXP ffi_dots_elt(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return r_env_dot_get(env, i);
}

SEXP ffi_dot_type(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    r_dot_type_t type = r_env_dot_type(env, i);

    switch (type) {
    case DOT_TYPE_value:   return Rf_mkString("value");
    case DOT_TYPE_missing: return Rf_mkString("missing");
    case DOT_TYPE_delayed: return Rf_mkString("delayed");
    case DOT_TYPE_forced:  return Rf_mkString("forced");
    default:               Rf_error("unreachable");
    }
}

SEXP ffi_dot_delayed_expr(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return r_env_dot_delayed_expr(env, i);
}

SEXP ffi_dot_delayed_env(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return r_env_dot_delayed_env(env, i);
}

SEXP ffi_dot_forced_expr(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return r_env_dot_forced_expr(env, i);
}
