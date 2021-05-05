#ifndef RLANG_INTERNAL_UTILS_H
#define RLANG_INTERNAL_UTILS_H


r_obj* new_preserved_empty_list();
r_obj* rlang_ns_get(const char* name);
r_obj* ffi_enquo(r_obj* sym, r_obj* frame);

extern r_obj* rlang_ns_env;

void signal_soft_deprecated(const char* msg, const char* id, r_obj* env);
void warn_deprecated(const char* id, const char* fmt, ...);
void stop_defunct(const char* fmt, ...);

bool r_is_prefixed_call(r_obj* x, const char* name);
bool r_is_namespaced_call(r_obj* x, const char* ns, const char* name);
bool r_is_namespaced_call_any(r_obj* x, const char* ns, const char** names, int n);

static inline
r_obj* r_nms_get(r_obj* nms, r_ssize i) {
  if (nms == r_null) {
    return r_strs.empty;
  } else {
    return r_chr_get(nms, i);
  }
}

r_obj* nms_are_duplicated(r_obj* nms, bool from_last);

bool vec_find_first_duplicate(r_obj* x, r_obj* except, r_ssize* index);

static inline
r_obj* r_vec_coerce(r_obj* x, enum r_type to) {
  return Rf_coerceVector(x, to);
}

r_obj* chr_detect_dups(r_obj* x);


#endif
