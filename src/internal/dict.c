#include <rlang.h>
#include "dict.h"
#include "xxhash/xxhash.h"

struct r_dict r_new_dict(r_ssize size) {
  struct r_dict dict;

  dict.buckets = r_new_vector(r_type_list, size);
  dict.p_buckets = r_list_deref_const(dict.buckets);

  dict.n_buckets = size;
  dict.n_entries = 0;

  return dict;
}

static
r_ssize dict_hash(const struct r_dict* dict, sexp* key) {
  uint64_t hash = XXH3_64bits(&key, sizeof(sexp*));
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

  sexp* node = KEEP(r_new_node(value, r_null));
  r_node_poke_tag(node, key);

  if (prev == r_null) {
    // Empty bucket
    r_list_poke(dict->buckets, i, node);
  } else {
    r_node_poke_cdr(prev, node);
  }

  FREE(1);
  return r_null;
}
