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

// Unwrap nested promises to the innermost one.
// Sets `*forced` to TRUE if the innermost promise is forced.
static SEXP delayed_promise_unwrap(SEXP elt, Rboolean *forced) {
    if (!is_promise(elt) || promise_is_forced(elt))
        Rf_error("internal: expected a delayed promise");

    while (is_promise(elt)) {
        if (promise_env(elt) == R_NilValue) {
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

    *forced = FALSE;
    return elt;
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

    if (!is_promise(elt))
        return DOT_TYPE_value;

    if (promise_is_forced(elt))
        return DOT_TYPE_forced;

    Rboolean forced;
    delayed_promise_unwrap(elt, &forced);
    if (forced)
        return DOT_TYPE_forced;

    return DOT_TYPE_delayed;
}

// R API: R_DotDelayedExpression
SEXP dot_delayed_expr(int i, SEXP env) {
    SEXP elt = dot_find(i, env);

    if (!is_promise(elt))
        Rf_error("not a delayed promise");
    if (promise_is_forced(elt))
        Rf_error("not a delayed promise");

    Rboolean forced;
    SEXP inner = delayed_promise_unwrap(elt, &forced);
    if (forced)
        Rf_error("not a delayed promise");

    return promise_expr(inner);
}

// R API: R_DotDelayedEnvironment
SEXP dot_delayed_env(int i, SEXP env) {
    SEXP elt = dot_find(i, env);

    if (!is_promise(elt))
        Rf_error("not a delayed promise");
    if (promise_is_forced(elt))
        Rf_error("not a delayed promise");

    Rboolean forced;
    SEXP inner = delayed_promise_unwrap(elt, &forced);
    if (forced)
        Rf_error("not a delayed promise");

    return promise_env(inner);
}

// R API: R_DotForcedExpression
SEXP dot_forced_expr(int i, SEXP env) {
    SEXP elt = dot_find(i, env);

    if (!is_promise(elt))
        Rf_error("not a forced promise");

    if (promise_is_forced(elt))
        return promise_expr(elt);

    Rboolean forced;
    SEXP inner = delayed_promise_unwrap(elt, &forced);
    if (forced)
        return promise_expr(inner);

    Rf_error("not a forced promise");
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
