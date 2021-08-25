#ifndef RLANG_NODE_H
#define RLANG_NODE_H


static inline r_obj* r_node_car(r_obj* x) { return CAR(x); }
static inline r_obj* r_node_cdr(r_obj* x) { return CDR(x); }
static inline r_obj* r_node_tag(r_obj* x) { return TAG(x); }
static inline r_obj* r_node_caar(r_obj* x) { return CAAR(x); }
static inline r_obj* r_node_cadr(r_obj* x) { return CADR(x); }
static inline r_obj* r_node_cdar(r_obj* x) { return CDAR(x); }
static inline r_obj* r_node_cddr(r_obj* x) { return CDDR(x); }

static inline void r_node_poke_car(r_obj* x, r_obj* newcar) { SETCAR(x, newcar); }
static inline void r_node_poke_cdr(r_obj* x, r_obj* newcdr) { SETCDR(x, newcdr); }
static inline void r_node_poke_tag(r_obj* x, r_obj* tag) { SET_TAG(x, tag); }
static inline void r_node_poke_caar(r_obj* x, r_obj* newcaar) { SETCAR(CAR(x), newcaar); }
static inline void r_node_poke_cadr(r_obj* x, r_obj* newcar) { SETCADR(x, newcar); }
static inline void r_node_poke_cdar(r_obj* x, r_obj* newcdar) { SETCDR(CAR(x), newcdar); }
static inline void r_node_poke_cddr(r_obj* x, r_obj* newcdr) { SETCDR(CDR(x), newcdr); }

static inline
r_obj* r_new_node(r_obj* car, r_obj* cdr) {
  return Rf_cons(car, cdr);
}
static inline
r_obj* r_new_node3(r_obj* car, r_obj* cdr, r_obj* tag) {
  r_obj* out = Rf_cons(car, cdr);
  SET_TAG(out, tag);
  return out;
}

r_obj* r_new_pairlist(const struct r_pair* args, int n, r_obj** tail);

#define r_pairlist Rf_list1
#define r_pairlist2 Rf_list2
#define r_pairlist3 Rf_list3
#define r_pairlist4 Rf_list4
#define r_pairlist5 Rf_list5

r_obj* r_pairlist_find(r_obj* node, r_obj* tag);
r_obj* r_pairlist_rev(r_obj* node);

static inline
r_obj* r_pairlist_get(r_obj* node, r_obj* tag) {
  return r_node_car(r_pairlist_find(node, tag));
}

static inline
r_obj* r_pairlist_tail(r_obj* x) {
  r_obj* cdr = r_null;
  while ((cdr = r_node_cdr(x)) != r_null) {
    x = cdr;
  }
  return x;
}

r_obj* r_node_tree_clone(r_obj* x);


#endif
