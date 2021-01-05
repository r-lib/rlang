#ifndef RLANG_INTERNAL_DICT_H
#define RLANG_INTERNAL_DICT_H

/**
 * This is a simple hash table of `sexp*`. It is structured like R
 * environments and uses xxhash for hashing.
 */


// Similar to environments: list of pairlist buckets
struct r_dict {
  sexp* buckets;

  // For retrieval only
  sexp* const * p_buckets;

  r_ssize n_buckets;
  r_ssize n_entries;
};

static inline
void KEEP_DICT(struct r_dict* dict, int* n) {
  KEEP(dict->buckets);
  *n += 1;
}

struct r_dict r_new_dict(r_ssize size);
sexp* r_dict_put(struct r_dict* dict, sexp* key, sexp* value);


#endif
