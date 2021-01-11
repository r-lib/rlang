#ifndef RLANG_INTERNAL_UTILS_H
#define RLANG_INTERNAL_UTILS_H


sexp* new_preserved_empty_list();
sexp* rlang_ns_get(const char* name);
sexp* rlang_enquo(sexp* sym, sexp* frame);

extern sexp* rlang_ns_env;

void signal_soft_deprecated(const char* msg, const char* id, sexp* env);
void warn_deprecated(const char* id, const char* fmt, ...);
void stop_defunct(const char* fmt, ...);

bool r_is_prefixed_call(sexp* x, const char* name);
bool r_is_namespaced_call(sexp* x, const char* ns, const char* name);
bool r_is_namespaced_call_any(sexp* x, const char* ns, const char** names, int n);

static inline
sexp* r_nms_get(sexp* nms, r_ssize i) {
  if (nms == r_null) {
    return r_strings_empty;
  } else {
    return r_chr_get(nms, i);
  }
}

sexp* nms_are_duplicated(sexp* nms, bool from_last);
sexp* r_new_list(sexp* x, const char* name);

bool vec_find_first_duplicate(sexp* x, sexp* except, r_ssize* index);

static inline
sexp* r_vec_coerce(sexp* x, enum r_type to) {
  return Rf_coerceVector(x, to);
}


#endif
