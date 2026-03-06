#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#include "rlang.h"
#include "internal/dots-api.h"

#define _(string) (string)

static int dotDotVal(SEXP sym);


// Capture implementation ---

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

static SEXP capture_delayed(SEXP expr, SEXP expr_env) {
    // Follow ..N references. This is the main difference with the accessors
    // from the R API which do follow promises but not individual `..N`
    // references.
    while (TYPEOF(expr) == SYMSXP) {
        int dd = dotDotVal(expr);
        if (dd <= 0)
            break;
        if (!r_env_dots_exist(expr_env))
            error(_("'...' used in an incorrect context"));
        if (dd > r_env_dots_length(expr_env))
            error(_("the ... list contains fewer than %d elements"), dd);
        if (r_env_dot_type(expr_env, dd) != DOT_TYPE_delayed)
            break;

        SEXP new_env = r_env_dot_delayed_env(expr_env, dd);
        expr = r_env_dot_delayed_expr(expr_env, dd);
        expr_env = new_env;
    }

    MARK_NOT_MUTABLE(expr);
    return new_captured_arg(expr, expr_env);
}

static SEXP env_dot_delayed_capture(SEXP env, int i) {
    return capture_delayed(
        r_env_dot_delayed_expr(env, i),
        r_env_dot_delayed_env(env, i)
    );
}

static SEXP env_binding_delayed_capture(SEXP env, SEXP sym) {
    return capture_delayed(
        r_env_binding_delayed_expr(env, sym),
        r_env_binding_delayed_env(env, sym)
    );
}

SEXP attribute_hidden rlang_capturearginfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    enum r_env_binding_type arg_type = r_env_binding_type(rho, install("arg"));

    // May be a literal if compiler did not wrap in a promise
    SEXP sym;
    if (arg_type == R_ENV_BINDING_TYPE_delayed) {
        sym = r_env_binding_delayed_expr(rho, install("arg"));
    } else {
        sym = r_env_get(rho, install("arg"));
    }

    if (TYPEOF(sym) != SYMSXP) {
        error(_("\"x\" must be an argument name"));
    }

    PROTECT(sym);

    SEXP frame = CAR(args);

    int dd = dotDotVal(sym);

    if (dd) {
        if (!r_env_dots_exist(frame)) {
            error(_("'...' used in an incorrect context"));
        }
        if (dd > r_env_dots_length(frame)) {
            error(_("the ... list contains fewer than %d elements"), dd);
        }

        r_dot_type_t type = r_env_dot_type(frame, dd);

        switch (type) {
        case DOT_TYPE_missing:
            UNPROTECT(1);
            return new_captured_literal(R_MissingArg);
        case DOT_TYPE_value:
        case DOT_TYPE_forced: {
            SEXP value = PROTECT(r_env_dot_get(frame, dd));
            SEXP result = new_captured_literal(value);
            UNPROTECT(2);
            return result;
        }
        case DOT_TYPE_delayed:
            UNPROTECT(1);
            return env_dot_delayed_capture(frame, dd);
        default:
            r_stop_unreachable();
        }
    } else {
        SEXP found = r_env_until(frame, sym, R_EmptyEnv);

        if (found == R_EmptyEnv)
            error(_("object '%s' not found"), CHAR(PRINTNAME(sym)));

        enum r_env_binding_type type = r_env_binding_type(found, sym);

        switch (type) {
        case R_ENV_BINDING_TYPE_missing:
            UNPROTECT(1);
            return new_captured_literal(R_MissingArg);
        case R_ENV_BINDING_TYPE_delayed: {
            SEXP result = env_binding_delayed_capture(found, sym);
            UNPROTECT(1);
            return result;
        }
        case R_ENV_BINDING_TYPE_forced:
        case R_ENV_BINDING_TYPE_value:
        case R_ENV_BINDING_TYPE_active: {
            SEXP value = PROTECT(r_env_get(found, sym));
            SEXP result = new_captured_literal(value);
            UNPROTECT(2);
            return result;
        }
        case R_ENV_BINDING_TYPE_unbound:
        default:
            r_stop_unreachable();
        }
    }

    r_stop_unreachable();
}

SEXP capturedots(SEXP frame) {
    int n = r_env_dots_length(frame);

    if (n < 0)
	error(_("'...' used in an incorrect context"));

    if (n == 0)
	return R_NilValue;

    SEXP names = PROTECT(r_env_dots_names(frame));
    SEXP out = PROTECT(cons(R_NilValue, R_NilValue));
    SEXP node = out;

    SEXP dot = R_NilValue;
    PROTECT_INDEX dot_pi;
    PROTECT_WITH_INDEX(dot, &dot_pi);

    for (int i = 1; i <= n; ++i) {
	r_dot_type_t type = r_env_dot_type(frame, i);
	SEXP nm = STRING_ELT(names, i - 1);
	SEXP tag = (nm == R_BlankString) ? R_NilValue : installChar(nm);

	switch (type) {
	case DOT_TYPE_missing:
	    dot = new_captured_literal(R_MissingArg);
	    break;

	case DOT_TYPE_value:
	case DOT_TYPE_forced: {
	    SEXP value = PROTECT(r_env_dot_get(frame, i));
	    dot = new_captured_literal(value);
	    UNPROTECT(1);
	    break;
	}

	case DOT_TYPE_delayed:
	    dot = env_dot_delayed_capture(frame, i);
	    break;
	}

	REPROTECT(dot, dot_pi);
	SETCDR(node, cons(dot, R_NilValue));
	SET_TAG(CDR(node), tag);

	node = CDR(node);
    }

    UNPROTECT(3);
    return CDR(out);
}

SEXP attribute_hidden rlang_capturedots(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP caller_env = CAR(args);
    return capturedots(caller_env);
}


static int dotDotVal(SEXP sym)
{
    if (TYPEOF(sym) != SYMSXP)
        return 0;

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
