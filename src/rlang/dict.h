#ifndef RLANG_DICT_H
#define RLANG_DICT_H

/**
 * This is a simple hash table of `r_obj*`. It is structured like R
 * environments and uses xxhash for hashing.
 */


struct r_dict {
  r_obj* shelter;

  /* private: */
  r_obj* buckets;
  r_obj* const * p_buckets;

  r_ssize n_buckets;
  r_ssize n_entries;

  // For testing collisions
  bool prevent_resize;
};

struct r_dict* r_new_dict(r_ssize size);

r_obj* r_dict_poke(struct r_dict* p_dict, r_obj* key, r_obj* value);
bool r_dict_put(struct r_dict* p_dict, r_obj* key, r_obj* value);
bool r_dict_del(struct r_dict* p_dict, r_obj* key);
bool r_dict_has(struct r_dict* p_dict, r_obj* key);
r_obj* r_dict_get(struct r_dict* p_dict, r_obj* key);
r_obj* r_dict_get0(struct r_dict* p_dict, r_obj* key);

// Pass a negative size to resize by the default growth factor
void r_dict_resize(struct r_dict* p_dict, r_ssize size);

r_obj* r_dict_as_df_list(struct r_dict* p_dict);
r_obj* r_dict_as_list(struct r_dict* p_dict);


struct r_dict_iterator {
  r_obj* shelter;
  r_obj* key;
  r_obj* value;

  /* private: */
  r_ssize i;
  r_ssize n;
  r_obj* const * v_buckets;
  r_obj* node;
};

struct r_dict_iterator* r_new_dict_iterator(struct r_dict* p_dict);
bool r_dict_next(struct r_dict_iterator* p_it);


#endif
