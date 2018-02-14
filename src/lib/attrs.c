#define R_NO_REMAP
#include <Rinternals.h>

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
  r_poke_attributes(x, attrs);
  r_node_poke_tag(attrs, tag);
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


/*
 * TODO:
 *
 * push: assumes there is no `class` attribute in the node list
 * merge: looks for `class` attribute first
 *
 */

// Caller must poke the object bit
sexp* r_node_push_classes(sexp* node, const char** tags, int n) {
  static sexp* class_sym = NULL;
  if (!class_sym) {
    class_sym = r_sym("class");
  }

  sexp* tags_chr = KEEP(r_new_character(tags, n));
  sexp* attrs = r_new_node(tags_chr, node);
  r_node_poke_tag(attrs, class_sym);

  FREE(1);
  return attrs;
}

void r_push_classes(sexp* x, const char** tags, int n) {
  sexp* attrs = r_get_attributes(x);
  attrs = r_node_push_classes(attrs, tags, n);
  SET_ATTRIB(x, attrs);
  SET_OBJECT(x, 1);
}
