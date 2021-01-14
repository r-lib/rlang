#ifndef RLANG_NODE_H
#define RLANG_NODE_H


static inline sexp* r_node_car(sexp* x) { return CAR(x); }
static inline sexp* r_node_cdr(sexp* x) { return CDR(x); }
static inline sexp* r_node_tag(sexp* x) { return TAG(x); }
static inline sexp* r_node_caar(sexp* x) { return CAAR(x); }
static inline sexp* r_node_cadr(sexp* x) { return CADR(x); }
static inline sexp* r_node_cdar(sexp* x) { return CDAR(x); }
static inline sexp* r_node_cddr(sexp* x) { return CDDR(x); }

static inline void r_node_poke_car(sexp* x, sexp* newcar) { SETCAR(x, newcar); }
static inline void r_node_poke_cdr(sexp* x, sexp* newcdr) { SETCDR(x, newcdr); }
static inline void r_node_poke_tag(sexp* x, sexp* tag) { SET_TAG(x, tag); }
static inline void r_node_poke_caar(sexp* x, sexp* newcaar) { SETCAR(CAR(x), newcaar); }
static inline void r_node_poke_cadr(sexp* x, sexp* newcar) { SETCADR(x, newcar); }
static inline void r_node_poke_cdar(sexp* x, sexp* newcdar) { SETCDR(CAR(x), newcdar); }
static inline void r_node_poke_cddr(sexp* x, sexp* newcdr) { SETCDR(CDR(x), newcdr); }

static inline
sexp* r_new_node(sexp* car, sexp* cdr) {
  return Rf_cons(car, cdr);
}
static inline
sexp* r_new_node3(sexp* car, sexp* cdr, sexp* tag) {
  sexp* out = Rf_cons(car, cdr);
  SET_TAG(out, tag);
  return out;
}

#define r_pairlist Rf_list1
#define r_pairlist2 Rf_list2
#define r_pairlist3 Rf_list3
#define r_pairlist4 Rf_list4
#define r_pairlist5 Rf_list5

sexp* r_pairlist_find(sexp* node, sexp* tag);
sexp* r_pairlist_rev(sexp* node);

static inline
sexp* r_pairlist_get(sexp* node, sexp* tag) {
  return r_node_car(r_pairlist_find(node, tag));
}

static inline
sexp* r_pairlist_tail(sexp* x) {
  sexp* cdr = r_null;
  while ((cdr = r_node_cdr(x)) != r_null) {
    x = cdr;
  }
  return x;
}

sexp* r_node_tree_clone(sexp* x);


#endif
