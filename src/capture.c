#include <Rinternals.h>

#define attribute_hidden
#define _(string) (string)


SEXP attribute_hidden new_captured_arg(SEXP x, SEXP env) {
    static SEXP nms = NULL;
    if (!nms) {
        nms = allocVector(STRSXP, 2);
        R_PreserveObject(nms);
        MARK_NOT_MUTABLE(nms);
        SET_STRING_ELT(nms, 0, mkChar("expr"));
        SET_STRING_ELT(nms, 1, mkChar("env"));
    }

    SEXP info = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(info, 0, x);
    SET_VECTOR_ELT(info, 1, env);
    setAttrib(info, R_NamesSymbol, nms);

    UNPROTECT(1);
    return info;
}
SEXP attribute_hidden new_captured_literal(SEXP x) {
    return new_captured_arg(x, R_EmptyEnv);
}

SEXP attribute_hidden new_captured_promise(SEXP x, SEXP env) {
    // If promise was optimised away, return the literal
    if (TYPEOF(x) != PROMSXP)
        return new_captured_literal(x);

    SEXP expr_env = R_NilValue;
    SEXP expr = x;
    while (TYPEOF(expr) == PROMSXP) {
        expr_env = PRENV(expr);
        expr = PREXPR(expr);
    }

    // Evaluated arguments are returned as literals
    if (expr_env == R_NilValue) {
        SEXP value = PROTECT(eval(x, env));
        SEXP arg = new_captured_literal(value);
        UNPROTECT(1);
        return arg;
    } else {
        MARK_NOT_MUTABLE(expr);
        return new_captured_arg(expr, expr_env);
    }
}

SEXP attribute_hidden rlang_capturearg(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = findVarInFrame3(rho, install("x"), TRUE);

    // Happens when argument is unwrapped from a promise by the compiler
    if (TYPEOF(arg) != PROMSXP)
        return new_captured_literal(arg);

    // Get promise in caller frame
    SEXP caller_env = CAR(args);
    SEXP sym = PREXPR(arg);
    if (TYPEOF(sym) != SYMSXP)
        error(_("\"x\" must be an argument name"));

    arg = findVarInFrame3(caller_env, sym, TRUE);

    if (arg == R_UnboundValue)
        error(_("object '%s' not found"), CHAR(PRINTNAME(sym)));
    else
        return new_captured_promise(arg, caller_env);
}

SEXP attribute_hidden rlang_capturedots(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP caller_env = CAR(args);

    SEXP dots = PROTECT(findVarInFrame3(caller_env, R_DotsSymbol, TRUE));

    if (dots == R_UnboundValue) {
        error(_("Must capture dots in a function where dots exist"));
    }
    if (dots == R_MissingArg) {
        UNPROTECT(1);
        return allocVector(VECSXP, 0);
    }

    int n_dots = length(dots);
    SEXP captured = PROTECT(allocVector(VECSXP, n_dots));

    SEXP names = PROTECT(allocVector(STRSXP, n_dots));
    Rboolean named = FALSE;

    int i = 0;
    while (dots != R_NilValue) {
        SEXP head = CAR(dots);

        SEXP dot;
        if (TYPEOF(head) == PROMSXP)
            dot = new_captured_promise(head, caller_env);
        else
            dot = new_captured_literal(head);

        SET_VECTOR_ELT(captured, i, dot);

        if (TAG(dots) != R_NilValue) {
            named = TRUE;
            SET_STRING_ELT(names, i, PRINTNAME(TAG(dots)));
        }

        ++i;
        dots = CDR(dots);
    }

    if (named)
        setAttrib(captured, R_NamesSymbol, names);

    UNPROTECT(3);
    return captured;
}
