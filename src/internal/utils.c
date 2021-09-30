#include <rlang.h>


r_obj* new_preserved_empty_list() {
  r_obj* empty_list = r_alloc_list(0);
  r_preserve(empty_list);
  r_mark_shared(empty_list);

  r_obj* nms = KEEP(r_alloc_character(0));
  r_attrib_poke_names(empty_list, nms);
  FREE(1);

  return empty_list;
}


/* For debugging with gdb or lldb. Exported as a C callable.
 * Usage with lldb:
 *
 * ```
 * // Full backtrace:
 * expr R_GetCCallable("rlang", "rlang_print_backtrace")(true)
 *
 * // Linear backtrace:
 * expr R_GetCCallable("rlang", "rlang_print_backtrace")(false)
 * ```
 */
void rlang_print_backtrace(bool full) {
  r_obj* env = KEEP(r_peek_frame());
  r_obj* trace = KEEP(r_parse_eval("rlang::trace_back()", env));

  const char* source = full ?
    "print(x, simplify = 'none')" :
    "print(x, simplify = 'branch')";
  r_obj* call = KEEP(r_parse(source));

  r_eval_with_x(call, trace, r_envs.base);

  FREE(3);
  return;
}


static r_obj* signal_soft_deprecated_call = NULL;
void signal_soft_deprecated(const char* msg,
                            const char* id,
                            r_obj* env) {
  id = id ? id : msg;
  env = env ? env : r_envs.empty;
  if (!msg) {
    r_abort("Internal error: NULL `msg` in r_signal_soft_deprecated()");
  }

  r_obj* msg_ = KEEP(r_chr(msg));
  r_obj* id_ = KEEP(r_chr(id));

  r_eval_with_xyz(signal_soft_deprecated_call, msg_, id_, env, r_envs.base);

  FREE(2);
}

#define BUFSIZE 8192
#define INTERP(BUF, FMT, DOTS)                  \
  {                                             \
    va_list dots;                               \
    va_start(dots, FMT);                        \
    vsnprintf(BUF, BUFSIZE, FMT, dots);         \
    va_end(dots);                               \
                                                \
    BUF[BUFSIZE - 1] = '\0';                    \
  }

static void signal_retirement(const char* source, const char* buf);
static r_obj* warn_deprecated_call = NULL;

void warn_deprecated(const char* id, const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);
  r_obj* msg_ = KEEP(r_chr(buf));

  id = id ? id : buf;
  r_obj* id_ = KEEP(r_chr(id));

  r_eval_with_xy(warn_deprecated_call, msg_, id_, r_envs.base);
  FREE(2);
}

void stop_defunct(const char* fmt, ...) {
  char buf[BUFSIZE];
  INTERP(buf, fmt, ...);

  signal_retirement("stop_defunct(msg = x)", buf);

  r_abort("Internal error: Unexpected return after `.Defunct()`");
}

static void signal_retirement(const char* source, const char* buf) {
  r_obj* call = KEEP(r_parse(source));
  r_obj* msg = KEEP(r_chr(buf));

  r_eval_with_x(call, msg, rlang_ns_env);

  FREE(2);
}


#define R_SUBSET_NAMES_N 4
static const char* r_subset_names[R_SUBSET_NAMES_N] = { "$", "@", "::", ":::" };

bool r_is_prefixed_call(r_obj* x, const char* name) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  }

  r_obj* head = r_node_car(x);
  if (!r_is_call_any(head, r_subset_names, R_SUBSET_NAMES_N)) {
    return false;
  }

  if (name) {
    r_obj* rhs = r_node_cadr(r_node_cdr(head));
    if (!r_is_symbol(rhs, name)) {
      return false;
    }
  }

  return true;
}

bool r_is_namespaced_call(r_obj* x, const char* ns, const char* name) {
  if (r_typeof(x) != LANGSXP) {
    return false;
  }

  r_obj* head = r_node_car(x);
  if (!r_is_call(head, "::")) {
    return false;
  }

  if (ns) {
    r_obj* lhs = r_node_cadr(head);
    if (!r_is_symbol(lhs, ns)) {
      return false;
    }
  }

  if (name) {
    r_obj* rhs = r_node_cadr(r_node_cdar(x));
    if (!r_is_symbol(rhs, name)) {
      return false;
    }
  }

  return true;
}

bool r_is_namespaced_call_any(r_obj* x, const char* ns,
                              const char** names, int n) {
  if (!r_is_namespaced_call(x, ns, NULL)) {
    return false;
  }

  r_obj* args = r_node_cdar(x);
  r_obj* sym = r_node_cadr(args);
  return r_is_symbol_any(sym, names, n);
}

r_obj* nms_are_duplicated(r_obj* nms, bool from_last) {
  if (r_typeof(nms) != R_TYPE_character) {
    r_abort("Internal error: Expected a character vector of names for checking duplication");
  }
  r_obj* dups = KEEP(Rf_duplicated(nms, from_last));

  r_ssize n = r_length(dups);
  int* p_dups = r_lgl_begin(dups);
  r_obj* const * p_nms = r_chr_cbegin(nms);

  for (r_ssize i = 0; i < n; ++i) {
    if (p_nms[i] == r_strs.empty || p_nms[i] == r_globals.na_str) {
      p_dups[i] = false;
    }
  }

  FREE(1);
  return dups;
}

bool vec_find_first_duplicate(r_obj* x, r_obj* except, r_ssize* index) {
  r_ssize idx;
  if (except) {
    idx = Rf_any_duplicated3(x, except, false);
  } else {
    idx = Rf_any_duplicated(x, false);
  }

  if (idx) {
    if (index) {
      *index = idx - 1;
    }
    return true;
  } else {
    return false;
  }
}

// Can use simple pointer hashing thanks to the string pool
r_obj* chr_detect_dups(r_obj* x) {
  if (r_typeof(x) != R_TYPE_character) {
    r_stop_internal("chr_detect_dups", "`x` must be a character vector.");
  }

  x = KEEP(r_obj_encode_utf8(x));

  // Sentinel for duplicates
  r_obj* dup_flag = r_strs.empty;

  r_ssize n = r_length(x);
  r_obj* const * v_data = r_chr_cbegin(x);

  struct r_dict* p_dict = r_new_dict(n);
  KEEP(p_dict->shelter);

  for (r_ssize i = 0; i < n; ++i) {
    r_obj* key = v_data[i];

    r_obj* val = r_dict_get0(p_dict, key);
    if (val == NULL) {
      r_dict_put(p_dict, key, r_null);
    } else if (val == r_null) {
      r_dict_poke(p_dict, key, dup_flag);
    }
  }

  r_obj* out = KEEP(r_alloc_logical(n));
  int* v_out = r_lgl_begin(out);

  for (r_ssize i = 0; i < n; ++i) {
    v_out[i] = r_dict_get(p_dict, v_data[i]) == dup_flag;
  }

  FREE(3);
  return out;
}

r_obj* ffi_peek_srcref() {
  if (R_Srcref) {
    return R_Srcref;
  } else {
    return rlang_syms.c_null;
  }
}

r_obj* ffi_has_local_precious_list() {
  return r_lgl(_r_use_local_precious_list);
}
r_obj* ffi_use_local_precious_list(r_obj* x) {
  bool old = _r_use_local_precious_list;
  _r_use_local_precious_list = r_as_bool(x);
  return r_lgl(old);
}


void rlang_init_utils() {
  warn_deprecated_call = r_parse("rlang:::warn_deprecated(x, id = y)");
  r_preserve(warn_deprecated_call);

  signal_soft_deprecated_call = r_parse("rlang:::signal_soft_deprecated(x, id = y, env = z)");
  r_preserve(signal_soft_deprecated_call);
}
