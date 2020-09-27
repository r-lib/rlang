#include "rlang.h"

extern sexp* rlang_attrib(sexp* x);
extern sexp* r_poke_attrib(sexp* x, sexp* attrs);

sexp* r_attrib_push(sexp* x, sexp* tag, sexp* value) {
  sexp* attrs = r_new_node(value, r_attrib(x));
  r_node_poke_tag(attrs, tag);
  r_poke_attrib(x, attrs);
  return attrs;
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
sexp* r_node_push_classes(sexp* node, const char** tags) {
  sexp* tags_chr = KEEP(r_new_character(tags));
  sexp* attrs = r_new_node(tags_chr, node);
  r_node_poke_tag(attrs, r_syms_class);

  FREE(1);
  return attrs;
}
sexp* r_node_push_class(sexp* x, const char* tag) {
  static const char* tags[2] = { "", NULL };
  tags[0] = tag;
  return r_node_push_classes(x, tags);
}

void r_push_classes(sexp* x, const char** tags) {
  sexp* attrs = r_attrib(x);
  attrs = r_node_push_classes(attrs, tags);
  SET_ATTRIB(x, attrs);
  SET_OBJECT(x, 1);
}
void r_push_class(sexp* x, const char* tag) {
  static const char* tags[2] = { "", NULL };
  tags[0] = tag;
  r_push_classes(x, tags);
}
