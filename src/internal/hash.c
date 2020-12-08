#include <rlang.h>

/*
 * Using the standard xxhash defines, as seen in:
 * https://github.com/Cyan4973/xxHash/blob/4c881f796d6af27ef7d9c48f87817da0d3d75dc1/xxhash.c#L40-L41
 *
 * `XXH_INLINE_ALL` is an alternative, but it does not seem to be any faster for
 * our use cases. Additionally, on 32-bit Windows with Rtools40
 * (i.e. gcc 8.3.0), a significant warning is thrown when
 * `XXH_INLINE_ALL` is used.
 * https://github.com/Cyan4973/xxHash/issues/482
 */
#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION

#include "xxhash/xxhash.h"

#include <stdio.h> // sprintf()
#include <inttypes.h> // PRIx64

#ifdef R_VERSION
#  if (R_VERSION >= R_Version(3, 5, 0))
#    define USE_VERSION_3 1
#  else
#    define USE_VERSION_3 0
#  endif
#else
#  define USE_VERSION_3 0
#endif

/*
 * Before any R object data is serialized, `R_Serialize()` will first write out:
 *
 * Serialization info:
 * - 2 bytes for `"X\n"` to declare "binary" serialization (i.e. not "ascii")
 * - An `int` representing the serialization version
 * - An `int` representing `R_VERSION`
 * - An `int` representing the minimum R version where this serialization
 *   version was supported. This is `R_Version(2,3,0)` for version 2, and
 *   `R_Version(3,5,0)` for version 3.
 *
 * With version 3, it additionally writes out:
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

#if USE_VERSION_3
#  define N_BYTES_N_NATIVE_ENC (sizeof(int))
#endif

// -----------------------------------------------------------------------------

struct hash_state_t {
  bool skip;
  int n_skipped;
#if USE_VERSION_3
  int n_native_enc;
#endif
  XXH3_state_t* p_xx_state;
};

static inline struct hash_state_t new_hash_state(XXH3_state_t* p_xx_state);
static inline int hash_version();
static inline void hash_bytes(R_outpstream_t stream, void* p_input, int n);
static inline void hash_char(R_outpstream_t stream, int input);

sexp* rlang_hash(sexp* x) {
  XXH3_state_t* p_xx_state = XXH3_createState();
  XXH_errorcode err = XXH3_128bits_reset(p_xx_state);

  if (err == XXH_ERROR) {
    XXH3_freeState(p_xx_state);
    r_abort("Couldn't initialize hash state.");
  }

  struct hash_state_t state = new_hash_state(p_xx_state);

  int version = hash_version();

  // Unused
  sexp* (*hook)(sexp*, sexp*) = NULL;
  sexp* hook_data = r_null;

  struct R_outpstream_st stream;

  R_InitOutPStream(
    &stream,
    (R_pstream_data_t) &state,
    R_pstream_binary_format,
    version,
    hash_char,
    hash_bytes,
    hook,
    hook_data
  );

  R_Serialize(x, &stream);

  XXH128_hash_t hash = XXH3_128bits_digest(p_xx_state);
  XXH3_freeState(p_xx_state);

  // R assumes C99, so these are always defined as `uint64_t` in xxhash.h
  XXH64_hash_t high = hash.high64;
  XXH64_hash_t low = hash.low64;

  // 32 for hash, 1 for terminating null added by `sprintf()`
  char out[32 + 1];

  sprintf(out, "%" PRIx64 "%" PRIx64, high, low);

  return r_chr(out);
}

static inline
struct hash_state_t new_hash_state(XXH3_state_t* p_xx_state) {
  return (struct hash_state_t) {
    .skip = true,
    .n_skipped = 0,
#if USE_VERSION_3
    .n_native_enc = 0,
#endif
    .p_xx_state = p_xx_state
  };
}

static inline
int hash_version() {
#if USE_VERSION_3
  return 3;
#else
  return 2;
#endif
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
    XXH3_freeState(p_xx_state);
    r_abort("Couldn't update hash state.");
  }
}

static inline
void hash_char(R_outpstream_t stream, int input) {
  static int n = 1;
  void* p_input = (void*) (&input);
  hash_bytes(stream, p_input, n);
}

#if USE_VERSION_3

static inline
void hash_skip(struct hash_state_t* p_state, void* p_input, int n) {
  if (p_state->n_skipped == N_BYTES_SERIALIZATION_INFO) {
    // We've skipped all serialization info bytes.
    // Incoming bytes tell the size of the native encoding string.
    int* p_x = (int*) p_input;
    p_state->n_native_enc = *p_x;

    p_state->n_skipped += n;

    return;
  }

  p_state->n_skipped += n;

  // Haven't reached native encoding size yet,
  // there is no way we have skipped everything
  if (p_state->n_native_enc == 0) {
    return;
  }

  int n_bytes_header =
    N_BYTES_SERIALIZATION_INFO +
    N_BYTES_N_NATIVE_ENC +
    p_state->n_native_enc;

  if (p_state->n_skipped == n_bytes_header) {
    // We've skipped all serialization header bytes at this point
    p_state->skip = false;
  }
}

#else // !USE_VERSION_3

static inline
void hash_skip(struct hash_state_t* p_state, void* p_input, int n) {
  // Skip serialization header bytes
  p_state->n_skipped += n;

  if (p_state->n_skipped == N_BYTES_SERIALIZATION_INFO) {
    // We've skipped all serialization header bytes at this point
    p_state->skip = false;
  }
}

#endif // USE_VERSION_3

#undef USE_VERSION_3
