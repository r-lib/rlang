#ifndef RLANG_NODE_H
#define RLANG_NODE_H


static inline sexp* r_node_car(sexp* x) {
  return CAR(x);
}
static inline sexp* r_node_cdr(sexp* x) {
  return CDR(x);
}
static inline sexp* r_node_caar(sexp* x) {
  return CAAR(x);
}
static inline sexp* r_node_cadr(sexp* x) {
  return CADR(x);
}
static inline sexp* r_node_cdar(sexp* x) {
  return CDAR(x);
}
static inline sexp* r_node_cddr(sexp* x) {
  return CDDR(x);
}
static inline sexp* r_node_tail(sexp* x) {
  while (CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}

static inline sexp* r_node_poke_car(sexp* x, sexp* newcar) {
  SETCAR(x, newcar);
  return x;
}
static inline sexp* r_node_poke_cdr(sexp* x, sexp* newcdr) {
  SETCDR(x, newcdr);
  return x;
}
static inline sexp* r_node_poke_caar(sexp* x, sexp* newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
static inline sexp* r_node_poke_cadr(sexp* x, sexp* newcar) {
  SETCADR(x, newcar);
  return x;
}
static inline sexp* r_node_poke_cdar(sexp* x, sexp* newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
static inline sexp* r_node_poke_cddr(sexp* x, sexp* newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

static inline sexp* r_node_tag(sexp* x) {
  return TAG(x);
}
static inline sexp* r_node_poke_tag(sexp* x, sexp* tag) {SET_TAG(x, tag); return x;
}

static inline bool r_is_pairlist(sexp* x) {
  return TYPEOF(x) == LISTSXP;
}


static inline sexp* r_new_node(sexp* car, sexp* cdr) {
  return Rf_cons(car, cdr);
}

sexp* r_new_tagged_node(const char* tag, sexp* car, sexp* cdr);

static inline sexp* r_new_node_list(sexp* car) {
  return Rf_cons(car, r_null);
}
static inline sexp* r_new_node_list2(sexp* car1, sexp* car2) {
  sexp* out = KEEP(Rf_cons(car2, r_null));
  out = Rf_cons(car1, out);
  FREE(1);
  return out;
}

static inline sexp* r_build_node(sexp* car, sexp* cdr) {
  PROTECT(car);
  PROTECT(cdr);
  sexp* out = Rf_cons(car, cdr);
  UNPROTECT(2);
  return out;
}

static inline sexp* r_build_pairlist(sexp* car) {
  return r_build_node(car, r_null);
}
static inline sexp* r_build_pairlist2(sexp* car1, sexp* car2) {
  return r_build_node(car1, r_build_pairlist(car2));
}
static inline sexp* r_build_pairlist3(sexp* car1, sexp* car2, sexp* car3) {
  return r_build_node(car1, r_build_pairlist2(car2, car3));
}

sexp* r_node_tree_clone(sexp* x);


#endif
