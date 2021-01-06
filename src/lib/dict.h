#ifndef RLANG_DICT_H
#define RLANG_DICT_H

/**
 * This is a simple hash table of `sexp*`. It is structured like R
 * environments and uses xxhash for hashing.
 */


struct r_dict {
  sexp* shelter;

  // private:
  sexp* buckets;
  sexp* const * p_buckets;

  r_ssize n_buckets;
  r_ssize n_entries;
};

struct r_dict r_new_dict(r_ssize size);
sexp* r_dict_put(struct r_dict* dict, sexp* key, sexp* value);
bool r_dict_has(struct r_dict* dict, sexp* key);
sexp* r_dict_get(struct r_dict* dict, sexp* key);


#endif
