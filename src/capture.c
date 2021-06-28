#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#define attribute_hidden
#define _(string) (string)

static Rboolean dotDotVal(SEXP);
static SEXP capturedot(SEXP, int);


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
    SEXP expr_env = R_NilValue;

    SEXP expr = x;
    while (TYPEOF(expr) == PROMSXP) {
        expr_env = PRENV(expr);
        expr = PREXPR(expr);

	if (expr_env == R_NilValue)
	    break;

	if (TYPEOF(expr) == SYMSXP) {
	    int dd = dotDotVal(expr);
	    if (dd)
		expr = capturedot(expr_env, dd);
	}
    }

    // Evaluated arguments are returned as literals
    if (expr_env == R_NilValue) {
        SEXP value = PROTECT(eval(x, env));
        expr = new_captured_literal(value);
        UNPROTECT(1);
    } else {
        MARK_NOT_MUTABLE(expr);
        expr = new_captured_arg(expr, expr_env);
    }

    return expr;
}

SEXP attribute_hidden rlang_capturearginfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nProt = 0;

    // Unwrap first layer of promise
    SEXP sym = findVarInFrame3(rho, install("arg"), TRUE);
    PROTECT(sym); ++nProt;

    // May be a literal if compiler did not wrap in a promise
    if (TYPEOF(sym) != PROMSXP) {
	SEXP value = new_captured_literal(sym);
	UNPROTECT(nProt);
	return value;
    }

    sym = PREXPR(sym);

    if (TYPEOF(sym) != SYMSXP) {
        UNPROTECT(nProt);
        error(_("\"x\" must be an argument name"));
    }

    SEXP frame = CAR(args);
    SEXP arg;

    int dd = dotDotVal(sym);
    if (dd) {
	arg = capturedot(frame, dd);
    } else {
	arg = findVar(sym, frame);
	if (arg == R_UnboundValue)
	    error(_("object '%s' not found"), CHAR(PRINTNAME(sym)));
    }
    PROTECT(arg); ++nProt;

    SEXP value;
    if (arg == R_MissingArg)
        value = new_captured_literal(arg);
    else if (TYPEOF(arg) == PROMSXP)
        value = new_captured_promise(arg, frame);
    else
        value = new_captured_literal(arg);

    UNPROTECT(nProt);
    return value;
}

SEXP capturedots(SEXP frame) {
    SEXP dots = PROTECT(findVar(R_DotsSymbol, frame));

    if (dots == R_UnboundValue)
	error(_("'...' used in an incorrect context"));

    if (dots == R_MissingArg) {
        UNPROTECT(1);
        return R_NilValue;
    }

    SEXP out = PROTECT(cons(R_NilValue, R_NilValue));
    SEXP node = out;

    while (dots != R_NilValue) {
        SEXP head = CAR(dots);

        SEXP dot;
        if (TYPEOF(head) == PROMSXP)
            dot = new_captured_promise(head, frame);
        else
            dot = new_captured_literal(head);

        SETCDR(node, cons(dot, R_NilValue));
        SET_TAG(CDR(node), TAG(dots));

        node = CDR(node);
        dots = CDR(dots);
    }

    UNPROTECT(2);
    return CDR(out);
}

SEXP attribute_hidden rlang_capturedots(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP caller_env = CAR(args);
    return capturedots(caller_env);
}


static Rboolean dotDotVal(SEXP sym)
{
    const char* str = CHAR(PRINTNAME(sym));

    if (strlen(str) < 3)
	return 0;
    if (*str++ != '.')
	return 0;
    if (*str++ != '.')
	return 0;

    char* p_end;
    int val = (int) strtol(str, &p_end, 10);

    if (*p_end == '\0')
	return val;
    else
	return 0;
}

static SEXP capturedot(SEXP frame, int i) {
    if (i < 1)
	error("'i' must be a positive non-zero integer");

    SEXP dots = PROTECT(findVar(R_DotsSymbol, frame));
    if (dots == R_UnboundValue)
	error(_("'...' used in an incorrect context"));

    if (dots == R_MissingArg)
	goto fewer;

    for (int j = 1; j != i; ++j)
	dots = CDR(dots);

    if (dots == R_NilValue)
	goto fewer;

    UNPROTECT(1);
    return CAR(dots);

 fewer:
    error(_("the ... list contains fewer than %d elements"), i);
}


// Local Variables:
// tab-width: 8
// c-basic-offset: 4
// indent-tabs-mode: t
// End:
