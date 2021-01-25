#include <rlang.h>
#include "dict.h"

#define DICT_LOAD_THRESHOLD 0.75
#define DICT_GROWTH_FACTOR 2

static size_t size_round_power_2(size_t size);

#include "decl/dict-decl.h"


struct r_dict r_new_dict(r_ssize size) {
  if (size <= 0) {
    r_abort("`size` of dictionary must be positive.");
  }
  size = size_round_power_2(size);

  struct r_dict dict = { 0 };
  dict.shelter = KEEP(r_new_node(r_null, r_null));

  dict.buckets = r_new_vector(r_type_list, size);
  r_node_poke_car(dict.shelter, dict.buckets);

  dict.p_buckets = r_list_deref_const(dict.buckets);
  dict.n_buckets = size;

  FREE(1);
  return dict;
}

void r_dict_resize(struct r_dict* dict, r_ssize size) {
  if (size < 0) {
    size = dict->n_buckets * DICT_GROWTH_FACTOR;
  }
  struct r_dict new = r_new_dict(size);
  KEEP(new.shelter);

  r_ssize n = r_length(dict->buckets);
  sexp* const * p_buckets = dict->p_buckets;

  for (r_ssize i = 0; i < n; ++i) {
    sexp* bucket = p_buckets[i];

    while (bucket != r_null) {
      sexp* key = r_node_tag(bucket);
      sexp* value = r_node_car(bucket);
      r_dict_put(&new, key, value);

      bucket = r_node_cdr(bucket);
    }
  }

  // Update all data except the shelter which must stay valid for the
  // callers
  sexp* shelter = dict->shelter;
  r_node_poke_car(shelter, r_node_car(new.shelter));

  memcpy(dict, &new, sizeof(*dict));
  dict->shelter = shelter;

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
r_ssize dict_hash(const struct r_dict* dict, sexp* key) {
  uint64_t hash = r_xxh3_64bits(&key, sizeof(sexp*));
  return hash % dict->n_buckets;
}

// Returns `false` if `key` already exists in the dictionary, `true`
// otherwise
bool r_dict_put(struct r_dict* dict, sexp* key, sexp* value) {
  r_ssize hash;
  sexp* parent;
  sexp* node = dict_find_node_info(dict, key, &hash, &parent);

  if (node != r_null) {
    return false;
  }

  // Can't find `key` in the bucket. Create a new node.
  node = KEEP(r_new_node(value, r_null));
  r_node_poke_tag(node, key);

  if (parent == r_null) {
    // Empty bucket
    r_list_poke(dict->buckets, hash, node);
  } else {
    r_node_poke_cdr(parent, node);
  }

  ++dict->n_entries;

  float load = (float) dict->n_entries / (float) dict->n_buckets;
  if (!dict->prevent_resize && load > DICT_LOAD_THRESHOLD) {
    r_dict_resize(dict, -1);
  }

  FREE(1);
  return true;
}

// Returns `true` if key existed and was deleted. Returns `false` if
// the key could not be deleted because it did not exist in the dict.
bool r_dict_del(struct r_dict* dict, sexp* key) {
  r_ssize hash;
  sexp* parent;
  sexp* node = dict_find_node_info(dict, key, &hash, &parent);

  if (node == r_null) {
    return false;
  }

  if (parent == r_null) {
    r_list_poke(dict->buckets, hash, r_null);
  }  else {
    r_node_poke_cdr(parent, r_node_cdr(node));
  }

  return true;
}

bool r_dict_has(struct r_dict* dict, sexp* key) {
  return dict_find_node(dict, key) != r_null;
}

sexp* r_dict_get(struct r_dict* dict, sexp* key) {
  sexp* out = r_dict_get0(dict, key);

  if (!out) {
    r_abort("Can't find key in dictionary.");
  }

  return out;
}

/* The 0-suffixed variant returns a C `NULL` if the object doesn't
   exist. The regular variant throws an error in that case. */
sexp* r_dict_get0(struct r_dict* dict, sexp* key) {
  sexp* node = dict_find_node(dict, key);

  if (node == r_null) {
    return NULL;
  } else {
    return r_node_car(node);
  }
}

static
sexp* dict_find_node(struct r_dict* dict, sexp* key) {
  r_ssize i = dict_hash(dict, key);
  sexp* bucket = dict->p_buckets[i];

  while (bucket != r_null) {
    if (r_node_tag(bucket) == key) {
      return bucket;
    }
    bucket = r_node_cdr(bucket);
  }

  return r_null;
}

// Also returns hash and parent node if any
static
sexp* dict_find_node_info(struct r_dict* dict,
                          sexp* key,
                          r_ssize* hash,
                          sexp** parent) {
  r_ssize i = dict_hash(dict, key);
  *hash = i;

  sexp* bucket = dict->p_buckets[i];
  *parent = r_null;

  while (bucket != r_null) {
    if (r_node_tag(bucket) == key) {
      return bucket;
    }
    *parent = bucket;
    bucket = r_node_cdr(bucket);
  }

  return r_null;
}
