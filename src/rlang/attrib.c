#include "rlang.h"

sexp* r_attrib_push(sexp* x, sexp* tag, sexp* value) {
  sexp* attrs = r_new_node(value, r_attrib(x));
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
sexp* r_pairlist_clone_until(sexp* node, sexp* sentinel, sexp** parent_out) {
  sexp* parent = r_null;
  sexp* cur = node;
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

    sexp* tag = r_node_tag(cur);
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

  r_stop_unreached("r_pairlist_clone_until");
}


sexp* r_attrs_set_at(sexp* attrs, sexp* node, sexp* value) {
  sexp* sentinel = r_node_cdr(node);
  sexp* new_node = r_null;

  attrs = KEEP(r_pairlist_clone_until(attrs, sentinel, &new_node));
  r_node_poke_car(new_node, value);

  FREE(1);
  return attrs;
}
sexp* r_attrs_zap_at(sexp* attrs, sexp* node, sexp* value) {
  sexp* sentinel = node;
  sexp* new_node = r_null;

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
sexp* r_clone2(sexp* x) {
  sexp* attrs = r_attrib(x);

  // Prevent attributes from being cloned
  r_poke_attrib(x, r_null);
  sexp* out = r_clone(x);
  r_poke_attrib(x, attrs);
  r_poke_attrib(out, attrs);

  return out;
}

sexp* r_attrib_set(sexp* x, sexp* tag, sexp* value) {
  sexp* attrs = r_attrib(x);
  sexp* out = KEEP(r_clone2(x));

  sexp* node = attrs;
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
sexp* node_push_classes(sexp* node, const char** tags, r_ssize n) {
  sexp* tags_chr = KEEP(r_chr_n(tags, n));
  sexp* attrs = r_new_node(tags_chr, node);
  r_node_poke_tag(attrs, r_syms.class);

  FREE(1);
  return attrs;
}

void r_attrib_push_classes(sexp* x, const char** tags, r_ssize n) {
  sexp* attrs = r_attrib(x);
  attrs = node_push_classes(attrs, tags, n);
  SET_ATTRIB(x, attrs);
  SET_OBJECT(x, 1);
}
void r_attrib_push_class(sexp* x, const char* tag) {
  static const char* tags[1] = { "" };
  tags[0] = tag;
  r_attrib_push_classes(x, tags, 1);
}

bool r_is_named(sexp* x) {
  sexp* nms = r_names(x);

  if (r_typeof(nms) != R_TYPE_character) {
    return false;
  }

  if (r_chr_has(nms, "")) {
    return false;
  }

  return true;
}
