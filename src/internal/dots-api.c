#include "dots-api.h"

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


// Dots API - mirrors R-devel PR #209 ---
// See https://github.com/r-devel/r-svn/pull/209


// R API: R_DotsExist
Rboolean dots_exist(SEXP env) {
    SEXP dots = Rf_findVar(R_DotsSymbol, env);
    return dots != R_UnboundValue;
}

// R API: R_DotsLength
int dots_length(SEXP env) {
    SEXP dots = Rf_findVar(R_DotsSymbol, env);

    if (dots == R_UnboundValue)
        return -1;

    if (dots == R_MissingArg || TYPEOF(dots) != DOTSXP)
        return 0;

    return Rf_length(dots);
}

// R API: R_DotsNames
SEXP dots_names(SEXP env) {
    SEXP dots = Rf_findVar(R_DotsSymbol, env);

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

    UNPROTECT(1);
    return out;
}

static
SEXP dot_find(int i, SEXP env) {
    if (i <= 0)
        Rf_error("indexing '...' with non-positive index %d", i);

    SEXP dots = Rf_findVar(R_DotsSymbol, env);

    if (dots == R_UnboundValue)
        Rf_error("'...' used in an incorrect context");

    if (dots == R_MissingArg || TYPEOF(dots) != DOTSXP)
        Rf_error("the ... list contains fewer than %d elements", i);

    for (int j = 1; j < i; ++j) {
        dots = CDR(dots);
        if (dots == R_NilValue)
            Rf_error("the ... list contains fewer than %d elements", i);
    }

    return CAR(dots);
}

// R API: R_DotsElt
SEXP dots_elt(int i, SEXP env) {
    SEXP elt = dot_find(i, env);
    return Rf_eval(elt, env);
}

// R API: R_GetDotType
dot_type_t dot_type(int i, SEXP env) {
    SEXP elt = dot_find(i, env);

    if (elt == R_MissingArg)
        return DOT_TYPE_missing;

    if (is_promise(elt)) {
        if (promise_is_forced(elt))
            return DOT_TYPE_forced;
        else
            return DOT_TYPE_delayed;
    }

    return DOT_TYPE_value;
}

// R API: R_DotDelayedExpression
SEXP dot_delayed_expr(int i, SEXP env) {
    SEXP elt = dot_find(i, env);

    if (!is_promise(elt))
        Rf_error("not a delayed promise");

    // Check if outermost promise is forced - error in that case
    if (promise_is_forced(elt))
        Rf_error("not a delayed promise");

    // Unwrap nested promises until we get a non-promise expression
    // If we hit a forced INNER promise (PRENV == NULL), we still return its PREXPR
    while (is_promise(elt)) {
        SEXP expr_env = promise_env(elt);
        SEXP expr = promise_expr(elt);

        // Forced inner promise: PRENV is NULL, but we can still get PREXPR
        if (expr_env == R_NilValue) {
            return expr;
        }

        if (!is_promise(expr))
            return expr;
        elt = expr;
    }

    Rf_error("not a promise");
}

// R API: R_DotDelayedEnvironment
SEXP dot_delayed_env(int i, SEXP env) {
    SEXP elt = dot_find(i, env);
    SEXP expr_env = R_NilValue;

    if (!is_promise(elt))
        Rf_error("not a delayed promise");

    // Check if outermost promise is forced - error in that case
    if (promise_is_forced(elt))
        Rf_error("not a delayed promise");

    // Unwrap nested promises, tracking the environment of the innermost promise
    // If we hit a forced INNER promise (PRENV == NULL), return R_NilValue
    while (is_promise(elt)) {
        expr_env = promise_env(elt);
        SEXP expr = promise_expr(elt);

        // Forced inner promise: PRENV is NULL
        if (expr_env == R_NilValue) {
            return R_NilValue;
        }

        if (!is_promise(expr))
            return expr_env;
        elt = expr;
    }

    Rf_error("not a promise");
}

// R API: R_DotForcedExpression
SEXP dot_forced_expr(int i, SEXP env) {
    SEXP elt = dot_find(i, env);

    if (!is_promise(elt) || !promise_is_forced(elt))
        Rf_error("not a forced promise");

    return promise_expr(elt);
}


// FFI wrappers for R interface ---

SEXP ffi_dots_exist(SEXP env) {
    return Rf_ScalarLogical(dots_exist(env));
}

SEXP ffi_dots_length(SEXP env) {
    int n = dots_length(env);

    if (n < 0) {
        Rf_error("incorrect context: the current call has no '...' to look in");
    }

    return Rf_ScalarInteger(n);
}

SEXP ffi_dots_names(SEXP env) {
    return dots_names(env);
}

SEXP ffi_dots_elt(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return dots_elt(i, env);
}

SEXP ffi_dot_type(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    dot_type_t type = dot_type(i, env);

    switch (type) {
    case DOT_TYPE_value:   return Rf_mkString("value");
    case DOT_TYPE_missing: return Rf_mkString("missing");
    case DOT_TYPE_delayed: return Rf_mkString("delayed");
    case DOT_TYPE_forced:  return Rf_mkString("forced");
    default:               return Rf_mkString("value");
    }
}

SEXP ffi_dot_delayed_expr(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return dot_delayed_expr(i, env);
}

SEXP ffi_dot_delayed_env(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return dot_delayed_env(i, env);
}

SEXP ffi_dot_forced_expr(SEXP ffi_i, SEXP env) {
    int i = INTEGER(ffi_i)[0];
    return dot_forced_expr(i, env);
}
