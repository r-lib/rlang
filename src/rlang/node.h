#ifndef RLANG_NODE_H
#define RLANG_NODE_H


static inline SEXP r_node_car(SEXP x) {
  return CAR(x);
}
static inline SEXP r_node_cdr(SEXP x) {
  return CDR(x);
}
static inline SEXP r_node_caar(SEXP x) {
  return CAAR(x);
}
static inline SEXP r_node_cadr(SEXP x) {
  return CADR(x);
}
static inline SEXP r_node_cdar(SEXP x) {
  return CDAR(x);
}
static inline SEXP r_node_cddr(SEXP x) {
  return CDDR(x);
}
static inline SEXP r_node_tail(SEXP x) {
  while (CDR(x) != R_NilValue)
    x = CDR(x);
  return x;
}

static inline SEXP r_node_poke_car(SEXP x, SEXP newcar) {
  SETCAR(x, newcar);
  return x;
}
static inline SEXP r_node_poke_cdr(SEXP x, SEXP newcdr) {
  SETCDR(x, newcdr);
  return x;
}
static inline SEXP r_node_poke_caar(SEXP x, SEXP newcaar) {
  SETCAR(CAR(x), newcaar);
  return x;
}
static inline SEXP r_node_poke_cadr(SEXP x, SEXP newcar) {
  SETCADR(x, newcar);
  return x;
}
static inline SEXP r_node_poke_cdar(SEXP x, SEXP newcdar) {
  SETCDR(CAR(x), newcdar);
  return x;
}
static inline SEXP r_node_poke_cddr(SEXP x, SEXP newcdr) {
  SETCDR(CDR(x), newcdr);
  return x;
}

static inline SEXP r_node_tag(SEXP x) {
  return TAG(x);
}
static inline SEXP r_node_poke_tag(SEXP x, SEXP tag) {SET_TAG(x, tag); return x;
}

static inline bool r_is_pairlist(SEXP x) {
  return TYPEOF(x) == LISTSXP;
}


static inline SEXP r_new_node(SEXP car, SEXP cdr) {
  return Rf_cons(car, cdr);
}
static inline SEXP r_new_node_list(SEXP car) {
  return Rf_cons(car, r_null);
}

static inline SEXP r_build_node(SEXP car, SEXP cdr) {
  PROTECT(car);
  PROTECT(cdr);
  SEXP out = Rf_cons(car, cdr);
  UNPROTECT(2);
  return out;
}

static inline SEXP r_build_pairlist(SEXP car) {
  return r_build_node(car, r_null);
}
static inline SEXP r_build_pairlist2(SEXP car1, SEXP car2) {
  return r_build_node(car1, r_build_pairlist(car2));
}
static inline SEXP r_build_pairlist3(SEXP car1, SEXP car2, SEXP car3) {
  return r_build_node(car1, r_build_pairlist2(car2, car3));
}


#endif
