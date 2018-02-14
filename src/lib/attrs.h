#ifndef RLANG_ATTRS_H
#define RLANG_ATTRS_H


static inline sexp* r_get_attributes(sexp* x) {
  return ATTRIB(x);
}
static inline void r_poke_attributes(sexp* x, sexp* attrs) {
  SET_ATTRIB(x, attrs);
}

sexp* r_push_attribute(sexp* x, sexp* tag, sexp* value);
sexp* r_get_attribute(sexp* x, sexp* tag);

static inline void r_push_names(sexp* x, sexp* value) {
  r_push_attribute(x, R_NamesSymbol, value);
}

sexp* r_node_push_classes(sexp* x, const char** tags, int n);

static inline sexp* r_node_push_class(sexp* x, const char* tag) {
  return r_node_push_classes(x, &tag, 1);
}

void r_push_classes(sexp* x, const char** tags, int n);

static inline void r_push_class(sexp* x, const char* tag) {
  r_push_classes(x, &tag, 1);
}

#endif
