#include <Rinternals.h>

#define attribute_hidden
#define _(string) (string)


SEXP attribute_hidden capture_arg(SEXP x, SEXP env) {
    static SEXP nms = NULL;
    if (!nms) {
        nms = allocVector(STRSXP, 2);
        R_PreserveObject(nms);
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

SEXP attribute_hidden capture_promise(SEXP x) {
    SEXP env = R_NilValue;
    while (TYPEOF(x) == PROMSXP) {
        env = PRENV(x);
        x = PREXPR(x);
    }
    if (env == R_NilValue)
        error(_("the argument has already been evaluated"));
    if (NAMED(x) < 2)
        SET_NAMED(x, 2);

    return capture_arg(x, env);
}

SEXP attribute_hidden rlang_capturearg(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arg = findVarInFrame3(rho, install("x"), TRUE);

    if (TYPEOF(arg) == PROMSXP) {
        // Get promise in caller frame
        SEXP caller_env = CAR(args);
        SEXP sym = PREXPR(arg);
        if (TYPEOF(sym) != SYMSXP)
            error(_("\"x\" must be an argument name"));

        arg = findVarInFrame3(caller_env, sym, TRUE);
        return capture_promise(arg);
    } else {
        // Argument was optimised away
        return capture_arg(arg, R_EmptyEnv);
    }
}

SEXP attribute_hidden rlang_capturedots(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    // R code has checked for unbound dots
    SEXP caller_env = CAR(args);
    SEXP dots = findVarInFrame3(caller_env, R_DotsSymbol, TRUE);

    if (dots == R_MissingArg)
        return allocVector(VECSXP, 0);

    int n_dots = length(dots);
    SEXP captured = PROTECT(allocVector(VECSXP, n_dots));
    SEXP names = PROTECT(allocVector(STRSXP, n_dots));
    setAttrib(captured, R_NamesSymbol, names);

    SEXP dot;
    int i = 0;
    while (i != n_dots) {
        dot = CAR(dots);

        if (TYPEOF(dot) == PROMSXP) {
            dot = capture_promise(dot);
        } else {
            dot = capture_arg(dot, R_EmptyEnv);
        }
        SET_VECTOR_ELT(captured, i, dot);

        if (TAG(dots) != R_NilValue)
            SET_STRING_ELT(names, i, PRINTNAME(TAG(dots)));

        ++i;
        dots = CDR(dots);
    }

    UNPROTECT(2);
    return captured;
}
