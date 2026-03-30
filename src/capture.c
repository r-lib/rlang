#include <stdlib.h>
#include <string.h>

#include "rlang.h"

static int dotDotVal(r_obj* sym);


// Capture implementation ---

static
r_obj* new_captured_arg(r_obj* x, r_obj* env) {
    static r_obj* nms = NULL;
    if (!nms) {
        nms = r_alloc_character(2);
        r_preserve_global(nms);
        MARK_NOT_MUTABLE(nms);
        r_chr_poke(nms, 0, r_str("expr"));
        r_chr_poke(nms, 1, r_str("env"));
    }

    r_obj* info = KEEP(r_alloc_list(2));
    r_list_poke(info, 0, x);
    r_list_poke(info, 1, env);
    r_attrib_poke(info, r_syms.names, nms);

    FREE(1);
    return info;
}

static
r_obj* new_captured_literal(r_obj* x) {
    return new_captured_arg(x, r_envs.empty);
}

static
r_obj* capture_delayed(r_obj* expr, r_obj* expr_env) {
    // Follow ..N references. This is the main difference with the accessors
    // from the R API which do follow promises but not individual `..N`
    // references.
    while (r_typeof(expr) == R_TYPE_symbol) {
        int dd = dotDotVal(expr);
        if (dd <= 0) {
            break;
        }
        expr_env = r_env_until_dots(expr_env);
        if (!r_env_dots_exist(expr_env)) {
            r_abort("'...' used in an incorrect context");
        }
        if (dd > r_env_dots_length(expr_env)) {
            r_abort("the ... list contains fewer than %d elements", dd);
        }

        r_dot_type_t type = r_env_dot_type(expr_env, dd - 1);

        switch (type) {
        case DOT_TYPE_missing:
            return new_captured_literal(r_syms.missing);
        case DOT_TYPE_value:
        case DOT_TYPE_forced: {
            r_obj* value = KEEP(r_env_dot_get(expr_env, dd - 1));
            r_obj* result = new_captured_literal(value);
            FREE(1);
            return result;
        }
        case DOT_TYPE_delayed: {
            r_obj* new_env = r_env_dot_delayed_env(expr_env, dd - 1);
            expr = r_env_dot_delayed_expr(expr_env, dd - 1);
            expr_env = new_env;
            break;
        }
        default:
            r_stop_unreachable();
        }
    }

    MARK_NOT_MUTABLE(expr);
    return new_captured_arg(expr, expr_env);
}

static
r_obj* env_dot_delayed_capture(r_obj* env, r_ssize i) {
    return capture_delayed(
        r_env_dot_delayed_expr(env, i),
        r_env_dot_delayed_env(env, i)
    );
}

static
r_obj* env_binding_delayed_capture(r_obj* env, r_obj* sym) {
    return capture_delayed(
        r_env_binding_delayed_expr(env, sym),
        r_env_binding_delayed_env(env, sym)
    );
}

r_obj* rlang_capturearginfo(r_obj* call, r_obj* op, r_obj* args, r_obj* rho)
{
    enum r_env_binding_type arg_type = r_env_binding_type(rho, r_sym("arg"));

    // May be a literal if compiler did not wrap in a promise
    r_obj* sym;
    if (arg_type == R_ENV_BINDING_TYPE_delayed) {
        sym = r_env_binding_delayed_expr(rho, r_sym("arg"));
    } else {
        sym = r_env_get(rho, r_sym("arg"));
    }

    if (r_typeof(sym) != R_TYPE_symbol) {
        r_abort("\"x\" must be an argument name");
    }

    KEEP(sym);

    r_obj* frame = r_node_car(args);

    int dd = dotDotVal(sym);

    if (dd) {
        frame = r_env_until_dots(frame);
        if (!r_env_dots_exist(frame)) {
            r_abort("'...' used in an incorrect context");
        }
        if (dd > r_env_dots_length(frame)) {
            r_abort("the ... list contains fewer than %d elements", dd);
        }

        r_dot_type_t type = r_env_dot_type(frame, dd - 1);

        switch (type) {
        case DOT_TYPE_missing:
            FREE(1);
            return new_captured_literal(r_syms.missing);
        case DOT_TYPE_value:
        case DOT_TYPE_forced: {
            r_obj* value = KEEP(r_env_dot_get(frame, dd - 1));
            r_obj* result = new_captured_literal(value);
            FREE(2);
            return result;
        }
        case DOT_TYPE_delayed:
            FREE(1);
            return env_dot_delayed_capture(frame, dd - 1);
        default:
            r_stop_unreachable();
        }
    } else {
        r_obj* found = r_env_until(frame, sym, r_envs.empty);

        if (found == r_envs.empty) {
            r_abort("object '%s' not found", CHAR(r_sym_string(sym)));
        }

        enum r_env_binding_type type = r_env_binding_type(found, sym);

        switch (type) {
        case R_ENV_BINDING_TYPE_missing:
            FREE(1);
            return new_captured_literal(r_syms.missing);
        case R_ENV_BINDING_TYPE_delayed: {
            r_obj* result = env_binding_delayed_capture(found, sym);
            FREE(1);
            return result;
        }
        case R_ENV_BINDING_TYPE_forced:
        case R_ENV_BINDING_TYPE_value:
        case R_ENV_BINDING_TYPE_active: {
            r_obj* value = KEEP(r_env_get(found, sym));
            r_obj* result = new_captured_literal(value);
            FREE(2);
            return result;
        }
        case R_ENV_BINDING_TYPE_unbound:
        default:
            r_stop_unreachable();
        }
    }

    r_stop_unreachable();
}

r_obj* capturedots(r_obj* frame) {
    frame = r_env_until_dots(frame);
    if (!r_env_dots_exist(frame)) {
        r_stop_internal("Expected `...` in scope");
    }

    r_ssize n = r_env_dots_length(frame);

    if (n == 0) {
        return r_null;
    }

    r_obj* names = KEEP(r_env_dots_names(frame));
    r_obj* out = KEEP(r_new_node(r_null, r_null));
    r_obj* node = out;

    r_obj* dot = r_null;
    r_keep_loc dot_pi;
    KEEP_HERE(dot, &dot_pi);

    for (r_ssize i = 0; i < n; ++i) {
        r_dot_type_t type = r_env_dot_type(frame, i);
        r_obj* tag = r_null;
        if (names != r_null) {
            r_obj* nm = r_chr_get(names, i);
            if (nm != r_strs.empty) {
                tag = r_str_as_symbol(nm);
            }
        }

        switch (type) {
        case DOT_TYPE_missing:
            dot = new_captured_literal(r_syms.missing);
            break;

        case DOT_TYPE_value:
        case DOT_TYPE_forced: {
            r_obj* value = KEEP(r_env_dot_get(frame, i));
            dot = new_captured_literal(value);
            FREE(1);
            break;
        }

        case DOT_TYPE_delayed:
            dot = env_dot_delayed_capture(frame, i);
            break;

        default:
            r_stop_unreachable();
        }

        KEEP_AT(dot, dot_pi);
        r_node_poke_cdr(node, r_new_node(dot, r_null));
        r_node_poke_tag(r_node_cdr(node), tag);

        node = r_node_cdr(node);
    }

    FREE(3);
    return r_node_cdr(out);
}

r_obj* rlang_capturedots(r_obj* call, r_obj* op, r_obj* args, r_obj* rho)
{
    r_obj* caller_env = r_node_car(args);
    return capturedots(caller_env);
}


static
int dotDotVal(r_obj* sym)
{
    if (r_typeof(sym) != R_TYPE_symbol) {
        return 0;
    }

    const char* str = CHAR(r_sym_string(sym));

    if (strlen(str) < 3) {
        return 0;
    }
    if (*str++ != '.') {
        return 0;
    }
    if (*str++ != '.') {
        return 0;
    }

    char* p_end;
    int val = (int) strtol(str, &p_end, 10);

    if (*p_end == '\0') {
        return val;
    } else {
        return 0;
    }
}
