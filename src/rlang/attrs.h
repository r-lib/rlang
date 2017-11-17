#ifndef RLANG_ATTRS_H
#define RLANG_ATTRS_H


static inline SEXP r_get_attributes(SEXP x) {
  return ATTRIB(x);
}
static inline void r_poke_attributes(SEXP x, SEXP attrs) {
  SET_ATTRIB(x, attrs);
}


SEXP r_node_push_classes(SEXP node, const char** tags, int n);

static inline SEXP r_node_push_class(SEXP node, const char* tag) {
  return r_node_push_classes(node, &tag, 1);
}

static inline void r_push_classes(SEXP object, const char** tags, int n) {
  SEXP attrs = r_get_attributes(object);
  attrs = r_node_push_classes(attrs, tags, n);
  SET_ATTRIB(object, attrs);
  SET_OBJECT(object, 1);
}
static inline void r_push_class(SEXP object, const char* tag) {
  r_push_classes(object, &tag, 1);
}

#endif
