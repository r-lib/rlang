#include <rlang.h>
#include "dict.h"

#define DICT_LOAD_THRESHOLD 0.75
#define DICT_GROWTH_FACTOR 2

static size_t size_round_power_2(size_t size);


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

// Returns r_null if successful, the existing object otherwise. The
// key is compared by pointer.
sexp* r_dict_put(struct r_dict* dict, sexp* key, sexp* value) {
  r_ssize i = dict_hash(dict, key);

  sexp* bucket = dict->p_buckets[i];
  sexp* prev = r_null;

  while (bucket != r_null) {
    if (r_node_tag(bucket) == key) {
      return r_node_car(bucket);
    }
    prev = bucket;
    bucket = r_node_cdr(bucket);
  }

  // Can't find `key` in the bucket. Create a new node.
  sexp* node = KEEP(r_new_node(value, r_null));
  r_node_poke_tag(node, key);

  if (prev == r_null) {
    // Empty bucket
    r_list_poke(dict->buckets, i, node);
  } else {
    r_node_poke_cdr(prev, node);
  }

  ++dict->n_entries;

  float load = (float) dict->n_entries / (float) dict->n_buckets;
  if (!dict->prevent_resize && load > DICT_LOAD_THRESHOLD) {
    r_dict_resize(dict, -1);
  }

  FREE(1);
  return r_null;
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

bool r_dict_has(struct r_dict* dict, sexp* key) {
  return dict_find_node(dict, key) != r_null;
}

sexp* r_dict_get(struct r_dict* dict, sexp* key) {
  sexp* node = dict_find_node(dict, key);

  if (node == r_null) {
    r_abort("Can't find key in dictionary.");
  }

  return r_node_car(node);
}
