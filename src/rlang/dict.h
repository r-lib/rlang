#ifndef RLANG_DICT_H
#define RLANG_DICT_H

/**
 * This is a simple hash table of `sexp*`. It is structured like R
 * environments and uses xxhash for hashing.
 */


struct r_dict {
  sexp* shelter;

  /* private: */
  sexp* buckets;
  sexp* const * p_buckets;

  r_ssize n_buckets;
  r_ssize n_entries;

  // For testing collisions
  bool prevent_resize;
};

struct r_dict* r_new_dict(r_ssize size);

bool r_dict_put(struct r_dict* p_dict, sexp* key, sexp* value);
bool r_dict_del(struct r_dict* p_dict, sexp* key);
bool r_dict_has(struct r_dict* p_dict, sexp* key);
sexp* r_dict_get(struct r_dict* p_dict, sexp* key);
sexp* r_dict_get0(struct r_dict* p_dict, sexp* key);

// Pass a negative size to resize by the default growth factor
void r_dict_resize(struct r_dict* p_dict, r_ssize size);

sexp* r_dict_as_df_list(struct r_dict* p_dict);
sexp* r_dict_as_list(struct r_dict* p_dict);


struct r_dict_iterator {
  sexp* shelter;
  sexp* key;
  sexp* value;

  /* private: */
  r_ssize i;
  r_ssize n;
  sexp* const * v_buckets;
  sexp* node;
};

struct r_dict_iterator* r_new_dict_iterator(struct r_dict* p_dict);
bool r_dict_next(struct r_dict_iterator* p_it);


#endif
