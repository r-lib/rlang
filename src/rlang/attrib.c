#include "rlang.h"

r_obj* r_attrib_push(r_obj* x, r_obj* tag, r_obj* value) {
  r_obj* attrs = r_new_node(value, r_attrib(x));
  r_node_poke_tag(attrs, tag);
  r_poke_attrib(x, attrs);
  return attrs;
}

/**
 * - If `sentinel` is found in the first node: `parent_out` is `r_null`
 * - If `sentinel` is not found: both return value and `parent_out`
 *   are `r_null`
 * - If `sentinel` is `r_null`, this is like a full shallow duplication
 *   but returns tail node
 */
r_obj* r_pairlist_clone_until(r_obj* node, r_obj* sentinel, r_obj** parent_out) {
  r_obj* parent = r_null;
  r_obj* cur = node;
  int n_kept = 0;

  while (true) {
    if (cur == sentinel) {
      FREE(n_kept);
      *parent_out = parent;
      return node;
    }
    // Return NULL if sentinel is not found
    if (cur == r_null) {
      FREE(n_kept);
      *parent_out = r_null;
      return r_null;
    }

    r_obj* tag = r_node_tag(cur);
    cur = r_new_node(r_node_car(cur), r_node_cdr(cur));
    r_node_poke_tag(cur, tag);

    if (parent == r_null) {
      KEEP_N(cur, &n_kept);
      node = cur;
    } else {
      r_node_poke_cdr(parent, cur);
    }

    parent = cur;
    cur = r_node_cdr(cur);
  }

  r_stop_unreachable();
}


r_obj* r_attrs_set_at(r_obj* attrs, r_obj* node, r_obj* value) {
  r_obj* sentinel = r_node_cdr(node);
  r_obj* new_node = r_null;

  attrs = KEEP(r_pairlist_clone_until(attrs, sentinel, &new_node));
  r_node_poke_car(new_node, value);

  FREE(1);
  return attrs;
}
r_obj* r_attrs_zap_at(r_obj* attrs, r_obj* node, r_obj* value) {
  r_obj* sentinel = node;
  r_obj* new_node = r_null;

  attrs = KEEP(r_pairlist_clone_until(attrs, sentinel, &new_node));

  if (new_node == r_null) {
    // `node` is the first node of `attrs`
    attrs = r_node_cdr(attrs);
  } else {
    r_node_poke_cdr(new_node, r_node_cdr(node));
  }

  FREE(1);
  return attrs;
}
r_obj* r_clone2(r_obj* x) {
  r_obj* attrs = r_attrib(x);

  // Prevent attributes from being cloned
  r_poke_attrib(x, r_null);
  r_obj* out = r_clone(x);
  r_poke_attrib(x, attrs);
  r_poke_attrib(out, attrs);

  return out;
}

r_obj* r_attrib_set(r_obj* x, r_obj* tag, r_obj* value) {
  r_obj* attrs = r_attrib(x);
  r_obj* out = KEEP(r_clone2(x));

  r_obj* node = attrs;
  while (node != r_null) {
    if (r_node_tag(node) == tag) {
      if (value == r_null) {
        attrs = r_attrs_zap_at(attrs, node, value);
      } else {
        attrs = r_attrs_set_at(attrs, node, value);
      }
      r_poke_attrib(out, attrs);

      FREE(1);
      return out;
    }

    node = r_node_cdr(node);
  }

  if (value != r_null) {
    // Just add to the front if attribute does not exist yet
    attrs = KEEP(r_new_node(out, attrs));
    r_node_poke_tag(attrs, tag);
    r_node_poke_car(attrs, value);
    r_poke_attrib(out, attrs);
    FREE(1);
  }

  FREE(1);
  return out;
}


/**
 * With push_ prefix, assumes there is no `class` attribute in the
 * node list merge. This is for low-level construction of objects.
 */

// Caller must poke the object bit
static
r_obj* node_push_classes(r_obj* node, const char** tags, r_ssize n) {
  r_obj* tags_chr = KEEP(r_chr_n(tags, n));
  r_obj* attrs = r_new_node(tags_chr, node);
  r_node_poke_tag(attrs, r_syms.class_);

  FREE(1);
  return attrs;
}

void r_attrib_push_classes(r_obj* x, const char** tags, r_ssize n) {
  r_obj* attrs = r_attrib(x);
  attrs = node_push_classes(attrs, tags, n);
  SET_ATTRIB(x, attrs);
  SET_OBJECT(x, 1);
}
void r_attrib_push_class(r_obj* x, const char* tag) {
  static const char* tags[1] = { "" };
  tags[0] = tag;
  r_attrib_push_classes(x, tags, 1);
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
