#ifndef RLANG_ATTRS_H
#define RLANG_ATTRS_H


static inline SEXP r_get_attributes(SEXP x) {
  return ATTRIB(x);
}
static inline void r_poke_attributes(SEXP x, SEXP attrs) {
  SET_ATTRIB(x, attrs);
}

SEXP r_push_attribute(SEXP x, SEXP tag, SEXP value);

static inline void r_push_names(SEXP x, SEXP value) {
  r_push_attribute(x, R_NamesSymbol, value);
}

SEXP r_node_push_classes(SEXP x, const char** tags, int n);

static inline SEXP r_node_push_class(SEXP x, const char* tag) {
  return r_node_push_classes(x, &tag, 1);
}

void r_push_classes(SEXP x, const char** tags, int n);

static inline void r_push_class(SEXP x, const char* tag) {
  r_push_classes(x, &tag, 1);
}

#endif
