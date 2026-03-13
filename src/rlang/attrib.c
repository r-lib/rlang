#include "rlang.h"

static r_obj* r_attrib_get_cb(r_obj* tag, r_obj* val, void* data) {
  if (tag == *(r_obj**) data) {
    return val;
  }
  return NULL;
}

r_obj* r_attrib_get(r_obj* x, r_obj* tag) {
  r_obj* out = r_attrib_map(x, &r_attrib_get_cb, &tag);
  return out ? out : r_null;
}

// Collect attributes into a pairlist using `R_mapAttrib`
static r_obj* r_attrib_collect_cb(r_obj* tag, r_obj* val, void* data) {
  r_obj** p_tail = (r_obj**) data;

  r_obj* node = r_new_node(val, r_null);
  r_node_poke_tag(node, tag);
  r_node_poke_cdr(*p_tail, node);

  *p_tail = node;
  return NULL;
}

r_obj* r_attrib_collect(r_obj* x) {
  r_obj* sentinel = KEEP(r_new_node(r_null, r_null));
  r_obj* tail = sentinel;

  r_attrib_map(x, &r_attrib_collect_cb, &tail);

  FREE(1);
  return r_node_cdr(sentinel);
}

bool r_is_named(r_obj* x) {
  r_obj* nms = r_names(x);

  if (r_typeof(nms) != R_TYPE_character) {
    return false;
  }

  if (r_chr_has(nms, "")) {
    return false;
  }

  return true;
}

void r_attrib_poke_classes(r_obj* x, const char** classes, r_ssize n) {
  r_obj* classes_chr = KEEP(r_chr_n(classes, n));
  r_attrib_poke(x, r_syms.class_, classes_chr);
  FREE(1);
}
