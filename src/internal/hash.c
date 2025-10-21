#include <rlang.h>
#include "file.h"

/*
 * Using the standard xxhash defines, as seen in:
 * https://github.com/Cyan4973/xxHash/blob/4c881f796d6af27ef7d9c48f87817da0d3d75dc1/xxhash.c#L40-L41
 */
#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION

#include "xxhash/xxhash.h"

#include <stdio.h> // sprintf()
#include <inttypes.h> // PRIx64

#include "decl/hash-decl.h"

/*
 * Before any R object data is serialized, `R_Serialize()` will first write out:
 *
 * Serialization info:
 * - 2 bytes for `"X\n"` to declare "binary" serialization (i.e. not "ascii")
 * - An `int` representing the serialization version
 * - An `int` representing `R_VERSION`
 * - An `int` representing the minimum R version where this serialization
 *   version was supported. This is `R_Version(3,5,0)` for version 3.
 * - An `int` representing the `strlen()` of a `const char*` containing the
 *   native encoding.
 * - A `const char*` for that native encoding. The length of this comes from
 *   the previous `int` that was written out.
 *
 * Since this changes between R versions, we skip these first bytes before
 * streaming any data into the hashing algorithm.
 *
 * Reference to show where R appends this information:
 * https://github.com/wch/r-source/blob/d48ecd61012fa6ae645d087d9a6e97e200c32fbc/src/main/serialize.c#L1382-L1389
 */
#define N_BYTES_SERIALIZATION_INFO (2 + 3 * sizeof(int))
#define N_BYTES_N_NATIVE_ENC (sizeof(int))

// -----------------------------------------------------------------------------

struct exec_data {
  r_obj* x;
  XXH3_state_t* p_xx_state;
};

static r_obj* hash_impl(void* p_data);
static void hash_cleanup(void* p_data);

r_obj* ffi_hash(r_obj* x) {
  XXH3_state_t* p_xx_state = XXH3_createState();

  struct exec_data data = {
    .x = x,
    .p_xx_state = p_xx_state
  };

  return R_ExecWithCleanup(hash_impl, &data, hash_cleanup, &data);
}

struct hash_state_t {
  bool skip;
  int n_skipped;
  int n_native_enc;
  XXH3_state_t* p_xx_state;
};

static inline struct hash_state_t new_hash_state(XXH3_state_t* p_xx_state);
static inline int hash_version(void);
static inline r_obj* hash_value(XXH3_state_t* p_xx_state);
static inline void hash_bytes(R_outpstream_t stream, void* p_input, int n);
static inline void hash_char(R_outpstream_t stream, int input);

static
r_obj* hash_impl(void* p_data) {
  struct exec_data* p_exec_data = (struct exec_data*) p_data;
  r_obj* x = p_exec_data->x;
  XXH3_state_t* p_xx_state = p_exec_data->p_xx_state;

  XXH_errorcode err = XXH3_128bits_reset(p_xx_state);
  if (err == XXH_ERROR) {
    r_abort("Couldn't initialize hash state.");
  }

  struct hash_state_t state = new_hash_state(p_xx_state);

  int version = hash_version();

  // Unused
  r_obj* (*hook)(r_obj*, r_obj*) = NULL;
  r_obj* hook_data = r_null;

  // We use the unstructured binary format, rather than XDR, as that is faster.
  // In theory it may result in different hashes on different platforms, but
  // in practice only integers can have variable width and here they are 32 bit.
  R_pstream_format_t format = R_pstream_binary_format;

  struct R_outpstream_st stream;

  R_InitOutPStream(
    &stream,
    (R_pstream_data_t) &state,
    format,
    version,
    hash_char,
    hash_bytes,
    hook,
    hook_data
  );

  R_Serialize(x, &stream);

  r_obj* value = KEEP(hash_value(p_xx_state));
  r_obj* out = r_str_as_character(value);

  FREE(1);
  return out;
}

static
void hash_cleanup(void* p_data) {
  struct exec_data* p_exec_data = (struct exec_data*) p_data;
  XXH3_state_t* p_xx_state = p_exec_data->p_xx_state;
  XXH3_freeState(p_xx_state);
}

static inline
struct hash_state_t new_hash_state(XXH3_state_t* p_xx_state) {
  return (struct hash_state_t) {
    .skip = true,
    .n_skipped = 0,
    .n_native_enc = 0,
    .p_xx_state = p_xx_state
  };
}

static inline
int hash_version(void) {
  return 3;
}

static inline
r_obj* hash_value(XXH3_state_t* p_xx_state) {
  XXH128_hash_t hash = XXH3_128bits_digest(p_xx_state);

  // R assumes C99, so these are always defined as `uint64_t` in xxhash.h
  XXH64_hash_t high = hash.high64;
  XXH64_hash_t low = hash.low64;

  // 32 for hash, 1 for terminating null added by `snprintf()`
  char out[32 + 1];

  snprintf(out, sizeof(out), "%016" PRIx64 "%016" PRIx64, high, low);

  return r_str(out);
}

static inline void hash_skip(struct hash_state_t* p_state, void* p_input, int n);

static inline
void hash_bytes(R_outpstream_t stream, void* p_input, int n) {
  struct hash_state_t* p_state = (struct hash_state_t*) stream->data;

  if (p_state->skip) {
    hash_skip(p_state, p_input, n);
    return;
  }

  XXH3_state_t* p_xx_state = p_state->p_xx_state;
  XXH_errorcode err = XXH3_128bits_update(p_xx_state, p_input, n);

  if (err == XXH_ERROR) {
    r_abort("Couldn't update hash state.");
  }
}

static inline
void hash_char(R_outpstream_t stream, int input) {
  // `R_Serialize()` only ever calls `stream->OutChar()` for ASCII and
  // ASCIIHEX formats, neither of which we are using.
  // https://github.com/wch/r-source/blob/161e21346c024b79db2654d3331298f96cdf6968/src/main/serialize.c#L376
  r_stop_internal("Should never be called with binary format.");
}

static inline
void hash_skip(struct hash_state_t* p_state, void* p_input, int n) {
  if (p_state->n_skipped < N_BYTES_SERIALIZATION_INFO) {
    // Skip serialization info bytes
    p_state->n_skipped += n;
    return;
  }

  if (p_state->n_skipped == N_BYTES_SERIALIZATION_INFO) {
    // We've skipped all serialization info bytes.
    // Incoming bytes tell the size of the native encoding string.
    r_memcpy(&p_state->n_native_enc, p_input, sizeof(int));
    p_state->n_skipped += n;
    return;
  }

  p_state->n_skipped += n;

  int n_bytes_header =
    N_BYTES_SERIALIZATION_INFO +
    N_BYTES_N_NATIVE_ENC +
    p_state->n_native_enc;

  if (p_state->n_skipped == n_bytes_header) {
    // We've skipped all serialization header bytes at this point
    p_state->skip = false;
  }
}

// -----------------------------------------------------------------------------

r_obj* ffi_hash_file(r_obj* path) {
  XXH3_state_t* p_xx_state = XXH3_createState();

  struct exec_data data = {
    .x = path,
    .p_xx_state = p_xx_state
  };

  return R_ExecWithCleanup(hash_file_impl, &data, hash_cleanup, &data);
}

#define CHUNK_SIZE 512 * 1024

static
r_obj* hash_file_impl(void* p_data) {
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
  void* buf = (void*)R_alloc(CHUNK_SIZE, sizeof(char));

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

static inline
void hasher_finalizer(r_obj* x) {
  void* p_x = R_ExternalPtrAddr(x);

  if (!p_x) {
    // Defensively exit if the external pointer resolves to `NULL`
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
  return(out);
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
