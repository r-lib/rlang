#define R_NO_REMAP
#include <Rinternals.h>

// These change attributes in-place.

SEXP rlang_zap_attrs(SEXP x) {
  SET_ATTRIB(x, r_null);
  return x;
}

SEXP rlang_set_attrs(SEXP x, SEXP attrs) {
  SET_ATTRIB(x, attrs);
  return x;
}

SEXP rlang_get_attrs(SEXP x) {
  return ATTRIB(x);
}

SEXP r_push_attribute(SEXP x, SEXP tag, SEXP value) {
  SEXP attrs = r_new_node(value, r_get_attributes(x));
  r_poke_attributes(x, attrs);

  if (tag != r_null) {
    r_node_poke_tag(attrs, tag);
  }

  return attrs;
}


/*
 * TODO:
 *
 * push: assumes there is no `class` attribute in the node list
 * merge: looks for `class` attribute first
 *
 */

// Caller must poke the object bit
SEXP r_node_push_classes(SEXP node, const char** tags, int n) {
  static SEXP class_sym = NULL;
  if (!class_sym) {
    class_sym = r_sym("class");
  }

  SEXP tags_chr = KEEP(r_build_character(tags, n));
  SEXP attrs = r_new_node(tags_chr, node);
  r_node_poke_tag(attrs, class_sym);

  FREE(1);
  return attrs;
}

void r_push_classes(SEXP x, const char** tags, int n) {
  SEXP attrs = r_get_attributes(x);
  attrs = r_node_push_classes(attrs, tags, n);
  SET_ATTRIB(x, attrs);
  SET_OBJECT(x, 1);
}
