#include <rlang.h>
#define XXH_INLINE_ALL
#include "xxhash/xxhash.h"
#include <stdio.h> // sprintf()
#include <inttypes.h>

/*
 * Before writing any data from the user, a call to:
 * `serialize(ascii = FALSE, version = 2L)`
 * will first write out:
 *
 * - 2 bytes for `"X\n"` to declare "binary" serialization (i.e. not "ascii")
 * - An `int` representing the serialization version
 * - An `int` representing `R_VERSION`
 * - An `int` representing `R_Version(2,3,0)`
 *
 * Since this changes between R versions, we skip the first chunk of serialized
 * bytes corresponding to this information.
 *
 * Reference to show where R appends this information:
 * https://github.com/wch/r-source/blob/d48ecd61012fa6ae645d087d9a6e97e200c32fbc/src/main/serialize.c#L1382-L1389
 */
#define SKIP (2 + 3 * sizeof(int))

// -----------------------------------------------------------------------------

struct hash_state_t {
  bool skip;
  int n_skipped;
  XXH3_state_t* p_xx_state;
};

static inline void hash_bytes(R_outpstream_t stream, void* p_input, int n);
static inline void hash_char(R_outpstream_t stream, int input);

sexp* rlang_xxhash(sexp* x) {
  XXH3_state_t* p_xx_state = XXH3_createState();
  XXH_errorcode err = XXH3_128bits_reset(p_xx_state);

  if (err == XXH_ERROR) {
    XXH3_freeState(p_xx_state);
    r_abort("Couldn't initialize hash state.");
  }

  struct hash_state_t state = {
    .skip = true,
    .n_skipped = 0,
    .p_xx_state = p_xx_state
  };

  int version = 2;

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
void hash_bytes(R_outpstream_t stream, void* p_input, int n) {
  struct hash_state_t* p_state = (struct hash_state_t*) stream->data;

  if (p_state->skip) {
    // Skip serialization header bytes
    p_state->n_skipped += n;

    if (p_state->n_skipped == SKIP) {
      // We've skipped all serialization header bytes at this point
      p_state->skip = false;
    }

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
