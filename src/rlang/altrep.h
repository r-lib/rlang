#ifndef RLANG_ALTREP_H
#define RLANG_ALTREP_H


#if (R_VERSION < R_Version(3, 5, 0)) ||                 \
  (defined(_WIN32) && R_VERSION == R_Version(3, 5, 0))
# define R_HAS_ALTREP 0
#else
# define R_HAS_ALTREP 1
#endif

#if !R_HAS_ALTREP
# define ALTREP(x) false
#endif

#if R_VERSION >= R_Version(4, 3, 0)
#define RLANG_R_HAS_ALTLIST 1
#else
#define RLANG_R_HAS_ALTLIST 0
#endif

static inline
bool r_is_altrep(r_obj* x) {
  return ALTREP(x);
}

static inline
r_obj* r_altrep_data1(r_obj* x) {
  return R_altrep_data1(x);
}

static inline
r_obj* r_altrep_data2(r_obj* x) {
  return R_altrep_data2(x);
}


#endif
