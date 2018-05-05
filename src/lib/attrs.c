#include "rlang.h"

// These change attributes in-place.

sexp* rlang_zap_attrs(sexp* x) {
  SET_ATTRIB(x, r_null);
  return x;
}

sexp* rlang_set_attrs(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}

sexp* rlang_get_attrs(sexp* x) {
  return ATTRIB(x);
}

sexp* r_push_attribute(sexp* x, sexp* tag, sexp* value) {
  sexp* attrs = r_new_node(value, r_get_attributes(x));
  r_node_poke_tag(attrs, tag);
  r_poke_attributes(x, attrs);
  return attrs;
}

// Unlike Rf_getAttrib(), this never allocates
sexp* r_get_attribute(sexp* x, sexp* tag) {
  sexp* attrs = r_get_attributes(x);

  while (attrs != r_null) {
    if (r_node_tag(attrs) == tag) {
      sexp* attr = r_node_car(attrs);
      r_mark_shared(attr);
      return attr;
    }
    attrs = r_node_cdr(attrs);
  }

  return r_null;
}

sexp* r_set_attribute(sexp* x, sexp* sym, sexp* attr) {
  x = KEEP(r_duplicate(x, true));
  r_poke_attribute(x, sym, attr);

  FREE(1);
  return x;
}


/*
 * TODO:
 *
 * push: assumes there is no `class` attribute in the node list
 * merge: looks for `class` attribute first
 *
 */

// Caller must poke the object bit
sexp* r_node_push_classes(sexp* node, const char** tags) {
  sexp* tags_chr = KEEP(r_new_character(tags));
  sexp* attrs = r_new_node(tags_chr, node);
  r_node_poke_tag(attrs, r_class_sym);

  FREE(1);
  return attrs;
}
sexp* r_node_push_class(sexp* x, const char* tag) {
  static const char* tags[2] = { "", NULL };
  tags[0] = tag;
  return r_node_push_classes(x, tags);
}

void r_push_classes(sexp* x, const char** tags) {
  sexp* attrs = r_get_attributes(x);
  attrs = r_node_push_classes(attrs, tags);
  SET_ATTRIB(x, attrs);
  SET_OBJECT(x, 1);
}
void r_push_class(sexp* x, const char* tag) {
  static const char* tags[2] = { "", NULL };
  tags[0] = tag;
  r_push_classes(x, tags);
}
