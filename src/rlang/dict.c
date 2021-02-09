#include <rlang.h>
#include "dict.h"

#define DICT_LOAD_THRESHOLD 0.75
#define DICT_GROWTH_FACTOR 2

static size_t size_round_power_2(size_t size);

#include "decl/dict-decl.h"

static
sexp* classes_dict = NULL;


struct r_dict* r_new_dict(r_ssize size) {
  if (size <= 0) {
    r_abort("`size` of dictionary must be positive.");
  }
  size = size_round_power_2(size);

  sexp* shelter = KEEP(r_new_list(2));

  // TODO: r_new_raw0()
  sexp* dict_raw = r_new_raw(sizeof(struct r_dict));
  r_list_poke(shelter, 0, dict_raw);

  struct r_dict* p_dict = r_raw_deref(dict_raw);
  memset(p_dict, 0, sizeof(struct r_dict));

  p_dict->shelter = shelter;

  p_dict->buckets = r_new_list(size);
  r_list_poke(shelter, 1, p_dict->buckets);

  p_dict->p_buckets = r_list_deref_const(p_dict->buckets);
  p_dict->n_buckets = size;

  r_attrib_poke(shelter, r_syms_class, r_chr("rlang_dict"));

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
  sexp* const * p_buckets = p_dict->p_buckets;

  for (r_ssize i = 0; i < n; ++i) {
    sexp* bucket = p_buckets[i];

    while (bucket != r_null) {
      sexp* key = r_node_tag(bucket);
      sexp* value = r_node_car(bucket);
      r_dict_put(p_new_dict, key, value);

      bucket = r_node_cdr(bucket);
    }
  }

  // Update all data in place except the shelter and the raw sexp
  // which must stay validly protected by the callers
  sexp* old_shelter = p_dict->shelter;
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
r_ssize dict_hash(const struct r_dict* p_dict, sexp* key) {
  uint64_t hash = r_xxh3_64bits(&key, sizeof(sexp*));
  return hash % p_dict->n_buckets;
}

// Returns `false` if `key` already exists in the dictionary, `true`
// otherwise
bool r_dict_put(struct r_dict* p_dict, sexp* key, sexp* value) {
  r_ssize hash;
  sexp* parent;
  sexp* node = dict_find_node_info(p_dict, key, &hash, &parent);

  if (node != r_null) {
    return false;
  }

  // Can't find `key` in the bucket. Create a new node.
  node = KEEP(r_new_node(value, r_null));
  r_node_poke_tag(node, key);

  if (parent == r_null) {
    // Empty bucket
    r_list_poke(p_dict->buckets, hash, node);
  } else {
    r_node_poke_cdr(parent, node);
  }

  ++p_dict->n_entries;

  float load = (float) p_dict->n_entries / (float) p_dict->n_buckets;
  if (!p_dict->prevent_resize && load > DICT_LOAD_THRESHOLD) {
    r_dict_resize(p_dict, -1);
  }

  FREE(1);
  return true;
}

// Returns `true` if key existed and was deleted. Returns `false` if
// the key could not be deleted because it did not exist in the dict.
bool r_dict_del(struct r_dict* p_dict, sexp* key) {
  r_ssize hash;
  sexp* parent;
  sexp* node = dict_find_node_info(p_dict, key, &hash, &parent);

  if (node == r_null) {
    return false;
  }

  if (parent == r_null) {
    r_list_poke(p_dict->buckets, hash, r_null);
  }  else {
    r_node_poke_cdr(parent, r_node_cdr(node));
  }

  return true;
}

bool r_dict_has(struct r_dict* p_dict, sexp* key) {
  return dict_find_node(p_dict, key) != r_null;
}

sexp* r_dict_get(struct r_dict* p_dict, sexp* key) {
  sexp* out = r_dict_get0(p_dict, key);

  if (!out) {
    r_abort("Can't find key in dictionary.");
  }

  return out;
}

/* The 0-suffixed variant returns a C `NULL` if the object doesn't
   exist. The regular variant throws an error in that case. */
sexp* r_dict_get0(struct r_dict* p_dict, sexp* key) {
  sexp* node = dict_find_node(p_dict, key);

  if (node == r_null) {
    return NULL;
  } else {
    return r_node_car(node);
  }
}

static
sexp* dict_find_node(struct r_dict* p_dict, sexp* key) {
  r_ssize i = dict_hash(p_dict, key);
  sexp* bucket = p_dict->p_buckets[i];

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
sexp* dict_find_node_info(struct r_dict* p_dict,
                          sexp* key,
                          r_ssize* hash,
                          sexp** parent) {
  r_ssize i = dict_hash(p_dict, key);
  *hash = i;

  sexp* bucket = p_dict->p_buckets[i];
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


struct r_dict_iterator* r_new_dict_iterator(struct r_dict* p_dict) {
  sexp* shelter = r_new_raw(sizeof(struct r_dict_iterator));
  struct r_dict_iterator* p_it = r_raw_deref(shelter);

  p_it->shelter = shelter;
  p_it->key = r_null;
  p_it->value = r_null;
  p_it->i = 0;
  p_it->n = p_dict->n_buckets;
  p_it->v_buckets = p_dict->p_buckets;

  if (p_it->n == 0) {
    r_stop_internal("r_new_dict_iterator", "Empty dictionary.");
  }
  p_it->node = p_it->v_buckets[0];

  return p_it;
}

bool r_dict_it_next(struct r_dict_iterator* p_it) {
  if (p_it->v_buckets == NULL) {
    return false;
  }

  sexp* node = p_it->node;
  while (node == r_null) {
    r_ssize i = ++p_it->i;

    if (i >= p_it->n) {
      p_it->v_buckets = NULL;
      return false;
    }

    node = p_it->v_buckets[i];
    p_it->node = node;
  }

  p_it->key = r_node_tag(node);
  p_it->value = r_node_car(node);
  p_it->node = r_node_cdr(node);
  return true;
}
