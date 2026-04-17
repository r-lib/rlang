#include <rlang.h>
#include "file.h"

/*
 * Using the standard xxhash defines, as seen in:
 * https://github.com/Cyan4973/xxHash/blob/4c881f796d6af27ef7d9c48f87817da0d3d75dc1/xxhash.c#L40-L41
 */
#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION

#include "xxhash/xxhash.h"

#include <stdio.h>    // sprintf()
#include <inttypes.h> // PRIx64

struct hash_ctx {
    XXH3_state_t* p_state;
    bool zap_srcref;
};

static inline bool is_srcref_tag(r_obj* tag) {
    return tag == r_syms.srcref || tag == r_syms.srcfile ||
        tag == r_syms.wholeSrcref;
}

#include "decl/hash-decl.h"

// Finalize the hash state into a 128-bit digest and format it as a
// 32-character lowercase hex string (two 64-bit halves, zero-padded)
static inline r_obj* hash_value(XXH3_state_t* p_xx_state) {
    XXH128_hash_t hash = XXH3_128bits_digest(p_xx_state);
    XXH64_hash_t high = hash.high64;
    XXH64_hash_t low = hash.low64;
    char out[32 + 1];
    snprintf(out, sizeof(out), "%016" PRIx64 "%016" PRIx64, high, low);
    return r_str(out);
}

// Update the hash state with incoming bytes. Everything feeds through here.
static inline void hash_feed(
    XXH3_state_t* p_state,
    const void* data,
    size_t len
) {
    XXH_errorcode err = XXH3_128bits_update(p_state, data, len);
    if (err == XXH_ERROR) {
        r_abort("Can't update hash state.");
    }
}

static inline void hash_feed_int(XXH3_state_t* p_state, int x) {
    hash_feed(p_state, &x, sizeof(int));
}

static inline void hash_feed_ssize(XXH3_state_t* p_state, r_ssize x) {
    hash_feed(p_state, &x, sizeof(r_ssize));
}

static inline void hash_feed_ptr(XXH3_state_t* p_state, const void* ptr) {
    hash_feed(p_state, &ptr, sizeof(ptr));
}

// -----------------------------------------------------------------------------

// Feed the type tag, then user-visible data, then attributes
static void hash_object(struct hash_ctx* ctx, r_obj* x) {
    int type = r_typeof(x);
    hash_feed_int(ctx->p_state, type);

    // Vector-like S4 objects only differ from regular objects by the S4 bit. We
    // hash it here to disambiguate. This can be extended to a bitfield if needed.
    hash_feed_int(ctx->p_state, Rf_isS4(x));

    switch (type) {
    case R_TYPE_null:
        break;

    case R_TYPE_logical:
    case R_TYPE_integer:
    case R_TYPE_double:
    case R_TYPE_complex:
    case R_TYPE_raw:
    case R_TYPE_character:
    case R_TYPE_list:
    case R_TYPE_expression: {
        r_ssize n = r_length(x);
        hash_feed_ssize(ctx->p_state, n);
        hash_feed_vector_data(ctx, x, type, n);
        break;
    }

    case R_TYPE_pairlist:
    case R_TYPE_call:
    case R_TYPE_dots: {
        r_ssize n = r_length(x);

        // The parser stores srcref info as a 4th element on `function` calls.
        // When zapping srcrefs, stop after the 3rd node (formals, body, env).
        if (ctx->zap_srcref && type == R_TYPE_call &&
            r_node_car(x) == r_syms.function && n > 3) {
            n = 3;
        }

        hash_feed_ssize(ctx->p_state, n);
        r_obj* node = x;
        for (r_ssize i = 0; i < n; ++i, node = r_node_cdr(node)) {
            hash_object(ctx, r_node_tag(node));
            hash_object(ctx, r_node_car(node));
        }
        break;
    }

    case R_TYPE_closure: {
        hash_object(ctx, r_fn_formals(x));
        // `r_fn_body()` calls `R_ClosureExpr()` which unwraps bytecode,
        // so compiled and uncompiled closures hash identically.
        hash_object(ctx, r_fn_body(x));
        hash_feed_ptr(ctx->p_state, (const void*) r_fn_env(x));
        break;
    }

    // S4 objects that extend a basic type (e.g. `contains = "numeric"`)
    // have the SEXPTYPE of the underlying vector, so they are hashed by
    // the corresponding case above. Only pure S4 objects land here;
    // their slots are stored as attributes, which are walked below.
    case R_TYPE_s4:
        break;

    case R_TYPE_symbol: {
        const char* name = r_sym_c_string(x);
        int len = strlen(name);
        hash_feed_int(ctx->p_state, len);
        hash_feed(ctx->p_state, name, (size_t) len);
        break;
    }

    case R_TYPE_environment:
    case R_TYPE_builtin:
    case R_TYPE_special:
    case R_TYPE_pointer:
        hash_feed_ptr(ctx->p_state, (const void*) x);
        break;

    default:
        hash_feed_ptr(ctx->p_state, (const void*) x);
        break;
    }

    r_attrib_map(x, &hash_attribs_cb, ctx);
}

static r_obj* hash_attribs_cb(r_obj* tag, r_obj* value, void* data) {
    struct hash_ctx* ctx = (struct hash_ctx*) data;
    if (ctx->zap_srcref && is_srcref_tag(tag)) {
        return NULL;
    }
    hash_object(ctx, tag);
    hash_object(ctx, value);
    return NULL;
}

// -----------------------------------------------------------------------------

// Match `identical()` semantics: all NA variants -> NA_REAL,
// all NaN variants -> R_NaN, -0.0 -> +0.0
static inline double hash_normalise_dbl(double x) {
    if (R_IsNA(x)) {
        return NA_REAL;
    }
    if (R_IsNaN(x)) {
        return R_NaN;
    }
    if (x == 0.0) {
        return 0.0;
    }
    return x;
}

static void hash_feed_vector_data(
    struct hash_ctx* ctx,
    r_obj* x,
    int type,
    r_ssize n
) {
    switch (type) {
    case R_TYPE_logical:
    case R_TYPE_integer:
        hash_feed(ctx->p_state, r_vec_cbegin(x), (size_t) n * sizeof(int));
        break;

    case R_TYPE_double: {
        const double* p_x = r_dbl_cbegin(x);
        for (r_ssize i = 0; i < n; ++i) {
            double val = hash_normalise_dbl(p_x[i]);
            hash_feed(ctx->p_state, &val, sizeof(double));
        }
        break;
    }

    case R_TYPE_complex: {
        const r_complex* p_x = r_cpl_cbegin(x);
        for (r_ssize i = 0; i < n; ++i) {
            double re = hash_normalise_dbl(p_x[i].r);
            double im = hash_normalise_dbl(p_x[i].i);
            hash_feed(ctx->p_state, &re, sizeof(double));
            hash_feed(ctx->p_state, &im, sizeof(double));
        }
        break;
    }

    case R_TYPE_raw:
        hash_feed(ctx->p_state, r_raw_cbegin(x), (size_t) n);
        break;

    case R_TYPE_character: {
        r_obj* const* p_x = r_chr_cbegin(x);
        for (r_ssize i = 0; i < n; ++i) {
            r_obj* elt = p_x[i];
            if (elt == NA_STRING) {
                hash_feed_int(ctx->p_state, 1);
            } else {
                hash_feed_int(ctx->p_state, 0);
                int enc = (int) Rf_getCharCE(elt);
                int len = LENGTH(elt);
                hash_feed_int(ctx->p_state, enc);
                hash_feed_int(ctx->p_state, len);
                hash_feed(ctx->p_state, r_str_c_string(elt), (size_t) len);
            }
        }
        break;
    }

    case R_TYPE_list:
    case R_TYPE_expression:
        for (r_ssize i = 0; i < n; ++i) {
            hash_object(ctx, r_list_get(x, i));
        }
        break;

    default:
        r_stop_unreachable();
    }
}

// -----------------------------------------------------------------------------

struct exec_data {
    r_obj* x;
    XXH3_state_t* p_xx_state;
    bool zap_srcref;
};

r_obj* ffi_hash(r_obj* x, r_obj* ffi_zap_srcref) {
    XXH3_state_t* p_xx_state = XXH3_createState();

    struct exec_data data = {
        .x = x,
        .p_xx_state = p_xx_state,
        .zap_srcref = r_arg_as_bool(ffi_zap_srcref, "zap_srcref"),
    };

    return R_ExecWithCleanup(hash_impl, &data, hash_cleanup, &data);
}

static r_obj* hash_impl(void* p_data) {
    struct exec_data* p_exec_data = (struct exec_data*) p_data;
    r_obj* x = p_exec_data->x;
    XXH3_state_t* p_xx_state = p_exec_data->p_xx_state;

    XXH_errorcode err = XXH3_128bits_reset(p_xx_state);
    if (err == XXH_ERROR) {
        r_abort("Can't initialize hash state.");
    }

    struct hash_ctx ctx = {
        .p_state = p_xx_state,
        .zap_srcref = p_exec_data->zap_srcref,
    };

    hash_object(&ctx, x);

    r_obj* value = KEEP(hash_value(p_xx_state));
    r_obj* out = r_str_as_character(value);

    FREE(1);
    return out;
}

static void hash_cleanup(void* p_data) {
    struct exec_data* p_exec_data = (struct exec_data*) p_data;
    XXH3_state_t* p_xx_state = p_exec_data->p_xx_state;
    XXH3_freeState(p_xx_state);
}

// -----------------------------------------------------------------------------

r_obj* ffi_hash_file(r_obj* path) {
    XXH3_state_t* p_xx_state = XXH3_createState();

    struct exec_data data = {
        .x = path,
        .p_xx_state = p_xx_state,
        .zap_srcref = false,
    };

    return R_ExecWithCleanup(hash_file_impl, &data, hash_cleanup, &data);
}

#define CHUNK_SIZE 512 * 1024

static r_obj* hash_file_impl(void* p_data) {
    struct exec_data* p_exec_data = (struct exec_data*) p_data;
    r_obj* path = p_exec_data->x;
    XXH3_state_t* p_xx_state = p_exec_data->p_xx_state;

    if (r_typeof(path) != R_TYPE_character) {
        r_abort("`path` must be a character vector.");
    }

    r_ssize n_path = r_length(path);
    r_obj* const* v_path = r_chr_cbegin(path);

    r_obj* out = KEEP(r_alloc_character(n_path));

    // Allocate before opening file to avoid handle leak on allocation failure
    void* buf = (void*) R_alloc(CHUNK_SIZE, sizeof(char));

    for (r_ssize i = 0; i < n_path; ++i) {
        XXH_errorcode err = XXH3_128bits_reset(p_xx_state);
        if (err == XXH_ERROR) {
            r_abort("Can't initialize hash state.");
        }

        r_obj* elt = v_path[i];

        FILE* fp = r_fopen(elt, "rb");
        if (fp == NULL) {
            r_abort("Can't open file: %s.", Rf_translateChar(elt));
        }

        size_t n_read;

        while ((n_read = fread(buf, 1, CHUNK_SIZE, fp)) > 0) {
            XXH_errorcode err = XXH3_128bits_update(p_xx_state, buf, n_read);

            if (err == XXH_ERROR) {
                fclose(fp);
                r_abort("Can't update hash state.");
            }
        }

        fclose(fp);

        r_chr_poke(out, i, hash_value(p_xx_state));
    }

    FREE(1);
    return out;
}

#undef CHUNK_SIZE

// -----------------------------------------------------------------------------

static inline void hasher_finalizer(r_obj* x) {
    void* p_x = R_ExternalPtrAddr(x);

    if (!p_x) {
        return;
    }

    XXH3_state_t* p_xx_state = (XXH3_state_t*) p_x;
    XXH3_freeState(p_xx_state);

    R_ClearExternalPtr(x);
}

r_obj* ffi_hasher_init(void) {
    XXH3_state_t* p_xx_state = XXH3_createState();

    XXH_errorcode err = XXH3_128bits_reset(p_xx_state);
    if (err == XXH_ERROR) {
        r_abort("Can't initialize hash state.");
    }

    SEXP out = KEEP(R_MakeExternalPtr(p_xx_state, r_null, r_null));

    R_RegisterCFinalizerEx(out, hasher_finalizer, TRUE);

    FREE(1);
    return out;
}

r_obj* ffi_hasher_update(r_obj* x, r_obj* data) {
    if (r_typeof(x) != R_TYPE_pointer) {
        r_abort("`x` must be a hasher.");
    }

    if (r_typeof(data) != R_TYPE_raw) {
        r_abort("`data` must be a raw vector.");
    }

    void* p_x = R_ExternalPtrAddr(x);

    if (!p_x) {
        r_abort("`x` must be a hasher.");
    }

    XXH3_state_t* p_xx_state = (XXH3_state_t*) p_x;

    void* v_data = (void*) r_raw_begin(data);
    int size = r_ssize_as_integer(r_length(data));

    XXH_errorcode err = XXH3_128bits_update(p_xx_state, v_data, size);

    if (err == XXH_ERROR) {
        r_abort("Can't update hash state.");
    }

    return r_true;
}

r_obj* ffi_hasher_value(r_obj* x) {
    if (r_typeof(x) != R_TYPE_pointer) {
        r_abort("`x` must be a hasher.");
    }

    void* p_x = R_ExternalPtrAddr(x);

    if (!p_x) {
        r_abort("`x` must be a hasher.");
    }

    XXH3_state_t* p_xx_state = (XXH3_state_t*) p_x;

    return hash_value(p_xx_state);
}
