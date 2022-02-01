#include <rlang.h>
#include "dict.h"

#define DICT_LOAD_THRESHOLD 0.75
#define DICT_GROWTH_FACTOR 2

static size_t size_round_power_2(size_t size);

#include "decl/dict-decl.h"

#define DICT_DEREF(D) r_list_cbegin(D)

#define DICT_KEY(V) r_list_get(V, 0)
#define DICT_VALUE(V) r_list_get(V, 1)
#define DICT_CDR(V) r_list_get(V, 2)

#define DICT_POKE_KEY(D, K) r_list_poke(D, 0, K)
#define DICT_POKE_VALUE(D, V) r_list_poke(D, 1, V)
#define DICT_POKE_CDR(D, N) r_list_poke(D, 2, N)

#define V_DICT_KEY(V) (V)[0]
#define V_DICT_VALUE(V) (V)[1]
#define V_DICT_CDR(V) (V)[2]

static
r_obj* new_dict_node(r_obj* key, r_obj* value) {
  r_obj* bucket = r_alloc_list(3);
  DICT_POKE_KEY(bucket, key);
  DICT_POKE_VALUE(bucket, value);
  return bucket;
}

struct r_dict* r_new_dict(r_ssize size) {
  if (size <= 0) {
    r_abort("`size` of dictionary must be positive.");
  }
  size = size_round_power_2(size);

  r_obj* shelter = KEEP(r_alloc_list(2));

  r_obj* dict_raw = r_alloc_raw0(sizeof(struct r_dict));
  r_list_poke(shelter, 0, dict_raw);
  struct r_dict* p_dict = r_raw_begin(dict_raw);

  p_dict->shelter = shelter;

  p_dict->buckets = r_alloc_list(size);
  r_list_poke(shelter, 1, p_dict->buckets);

  p_dict->p_buckets = r_list_cbegin(p_dict->buckets);
  p_dict->n_buckets = size;

  r_attrib_poke(shelter, r_syms.class_, r_chr("rlang_dict"));

  FREE(1);
  return p_dict;
}

void r_dict_resize(struct r_dict* p_dict, r_ssize size) {
  if (size < 0) {
    size = p_dict->n_buckets * DICT_GROWTH_FACTOR;
  }
  struct r_dict* p_new_dict = r_new_dict(size);
  KEEP(p_new_dict->shelter);

  r_ssize n = r_length(p_dict->buckets);
  r_obj* const * p_buckets = p_dict->p_buckets;

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* bucket = p_buckets[i];

    while (bucket != r_null) {
      r_obj* const * v_bucket = DICT_DEREF(bucket);

      r_obj* key = V_DICT_KEY(v_bucket);
      r_obj* value = V_DICT_VALUE(v_bucket);
      r_dict_put(p_new_dict, key, value);

      bucket = V_DICT_CDR(v_bucket);
    }
  }

  // Update all data in place except the shelter and the raw sexp
  // which must stay validly protected by the callers
  r_obj* old_shelter = p_dict->shelter;
  r_list_poke(old_shelter, 1, r_list_get(p_new_dict->shelter, 1));

  memcpy(p_dict, p_new_dict, sizeof(*p_dict));
  p_dict->shelter = old_shelter;

  FREE(1);
}

static
size_t size_round_power_2(size_t size) {
    size_t out = 1;
    while (out < size) {
      out <<= 1;
    }
    return out;
}

static
r_ssize dict_hash(const struct r_dict* p_dict, r_obj* key) {
  uint64_t hash = r_xxh3_64bits(&key, sizeof(r_obj*));
  return hash % p_dict->n_buckets;
}

// Returns previous value of `key` if it existed or a C `NULL`
r_obj* r_dict_poke(struct r_dict* p_dict,
                   r_obj* key,
                   r_obj* value) {
  r_ssize hash;
  r_obj* parent;
  r_obj* node = dict_find_node_info(p_dict, key, &hash, &parent);

  if (node != r_null) {
    r_obj* old = DICT_VALUE(node);
    DICT_POKE_VALUE(node, value);
    return old;
  } else {
    dict_push(p_dict, hash, parent, key, value);
    return NULL;
  }
}

// Returns `false` if `key` already exists in the dictionary, `true`
// otherwise
bool r_dict_put(struct r_dict* p_dict, r_obj* key, r_obj* value) {
  r_ssize hash;
  r_obj* parent;
  r_obj* node = dict_find_node_info(p_dict, key, &hash, &parent);

  if (node != r_null) {
    return false;
  } else {
    dict_push(p_dict, hash, parent, key, value);
    return true;
  }
}

static
void dict_push(struct r_dict* p_dict,
               r_ssize hash,
               r_obj* parent,
               r_obj* key,
               r_obj* value) {
  r_obj* node = KEEP(new_dict_node(key, value));

  if (parent == r_null) {
    // Empty bucket
    r_list_poke(p_dict->buckets, hash, node);
  } else {
    DICT_POKE_CDR(parent, node);
  }

  ++p_dict->n_entries;

  float load = (float) p_dict->n_entries / (float) p_dict->n_buckets;
  if (!p_dict->prevent_resize && load > DICT_LOAD_THRESHOLD) {
    r_dict_resize(p_dict, -1);
  }

  FREE(1);
}

// Returns `true` if key existed and was deleted. Returns `false` if
// the key could not be deleted because it did not exist in the dict.
bool r_dict_del(struct r_dict* p_dict, r_obj* key) {
  r_ssize hash;
  r_obj* parent;
  r_obj* node = dict_find_node_info(p_dict, key, &hash, &parent);

  if (node == r_null) {
    return false;
  }

  r_obj* node_cdr = DICT_CDR(node);

  if (parent == r_null) {
    r_list_poke(p_dict->buckets, hash, node_cdr);
  } else {
    DICT_POKE_CDR(parent, node_cdr);
  }

  return true;
}

bool r_dict_has(struct r_dict* p_dict, r_obj* key) {
  return dict_find_node(p_dict, key) != r_null;
}

r_obj* r_dict_get(struct r_dict* p_dict, r_obj* key) {
  r_obj* out = r_dict_get0(p_dict, key);

  if (!out) {
    r_abort("Can't find key in dictionary.");
  }

  return out;
}

/* The 0-suffixed variant returns a C `NULL` if the object doesn't
   exist. The regular variant throws an error in that case. */
r_obj* r_dict_get0(struct r_dict* p_dict, r_obj* key) {
  r_obj* node = dict_find_node(p_dict, key);

  if (node == r_null) {
    return NULL;
  } else {
    return DICT_VALUE(node);
  }
}

static
r_obj* dict_find_node(struct r_dict* p_dict, r_obj* key) {
  r_ssize i = dict_hash(p_dict, key);
  r_obj* bucket = p_dict->p_buckets[i];

  while (bucket != r_null) {
    r_obj* const * v_bucket = DICT_DEREF(bucket);
    if (V_DICT_KEY(v_bucket) == key) {
      return bucket;
    }
    bucket = V_DICT_CDR(v_bucket);
  }

  return r_null;
}

// Also returns hash and parent node if any
static
r_obj* dict_find_node_info(struct r_dict* p_dict,
                           r_obj* key,
                           r_ssize* hash,
                           r_obj** parent) {
  r_ssize i = dict_hash(p_dict, key);
  *hash = i;

  r_obj* bucket = p_dict->p_buckets[i];
  *parent = r_null;

  while (bucket != r_null) {
    r_obj* const * v_bucket = DICT_DEREF(bucket);

    if (V_DICT_KEY(v_bucket) == key) {
      return bucket;
    }
    *parent = bucket;
    bucket = V_DICT_CDR(v_bucket);
  }

  return r_null;
}


struct r_dict_iterator* r_new_dict_iterator(struct r_dict* p_dict) {
  r_obj* shelter = r_alloc_raw(sizeof(struct r_dict_iterator));
  struct r_dict_iterator* p_it = r_raw_begin(shelter);

  p_it->shelter = shelter;
  p_it->key = r_null;
  p_it->value = r_null;
  p_it->i = 0;
  p_it->n = p_dict->n_buckets;
  p_it->v_buckets = p_dict->p_buckets;

  if (p_it->n == 0) {
    r_stop_internal("Empty dictionary.");
  }
  p_it->node = p_it->v_buckets[0];

  return p_it;
}

bool r_dict_next(struct r_dict_iterator* p_it) {
  if (p_it->v_buckets == NULL) {
    return false;
  }

  r_obj* node = p_it->node;
  while (node == r_null) {
    r_ssize i = ++p_it->i;

    if (i >= p_it->n) {
      p_it->v_buckets = NULL;
      return false;
    }

    node = p_it->v_buckets[i];
    p_it->node = node;
  }

  r_obj* const * v_node = DICT_DEREF(node);
  p_it->key = V_DICT_KEY(v_node);
  p_it->value = V_DICT_VALUE(v_node);
  p_it->node = V_DICT_CDR(v_node);
  return true;
}

static
const char* v_dict_it_df_names_c_strings[] = {
  "key",
  "value"
};
static
const enum r_type v_dict_it_df_types[] = {
  R_TYPE_list,
  R_TYPE_list
};
enum dict_it_df_locs {
  DICT_IT_DF_LOCS_key,
  DICT_IT_DF_LOCS_value
};
#define DICT_IT_DF_SIZE R_ARR_SIZEOF(v_dict_it_df_types)

r_obj* r_dict_as_df_list(struct r_dict* p_dict) {
  r_obj* nms = KEEP(r_chr_n(v_dict_it_df_names_c_strings,
                            DICT_IT_DF_SIZE));

  r_obj* out = KEEP(r_alloc_df_list(p_dict->n_entries,
                                    nms,
                                    v_dict_it_df_types,
                                    DICT_IT_DF_SIZE));

  r_obj* key = r_list_get(out, DICT_IT_DF_LOCS_key);
  r_obj* value = r_list_get(out, DICT_IT_DF_LOCS_value);

  struct r_dict_iterator* p_it = r_new_dict_iterator(p_dict);
  KEEP(p_it->shelter);

  for (r_ssize i = 0; r_dict_next(p_it); ++i) {
    r_list_poke(key, i, p_it->key);
    r_list_poke(value, i, p_it->value);
  }

  FREE(3);
  return out;
}
r_obj* r_dict_as_list(struct r_dict* p_dict) {
  r_obj* out = KEEP(r_alloc_list(p_dict->n_entries));

  struct r_dict_iterator* p_it = r_new_dict_iterator(p_dict);
  KEEP(p_it->shelter);

  for (r_ssize i = 0; r_dict_next(p_it); ++i) {
    r_list_poke(out, i, p_it->value);
  }

  FREE(2);
  return out;
}
