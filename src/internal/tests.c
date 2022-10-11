#include <rlang.h>

#include "decl/tests-decl.h"


struct r_test {
  const char* desc;
  bool (*fn_ptr)(void);
};

bool test_that_true_is_true(void) {
  if (true) return r_true; else return r_false;
}
bool test_that_false_is_false(void) {
  if (false) return r_false; else return r_true;
}

enum tests_df {
  TESTS_DF_desc = 0,
  TESTS_DF_fn_ptr,
  TESTS_DF_SIZE
};
static
const char* tests_df_names_c_strings[TESTS_DF_SIZE] = {
  [TESTS_DF_desc] = "desc",
  [TESTS_DF_fn_ptr] = "fn_ptr"
};
static
const enum r_type tests_df_types[TESTS_DF_SIZE] = {
  [TESTS_DF_desc] = R_TYPE_character,
  [TESTS_DF_fn_ptr] = R_TYPE_list
};


extern const struct r_test tests[];

r_obj* ffi_c_tests(void) {
  int n_rows = 0;
  while (tests[n_rows].desc) {
    ++n_rows;
  }

  r_obj* df = KEEP(r_alloc_df_list(n_rows,
                                   tests_df_names,
                                   tests_df_types,
                                   TESTS_DF_SIZE));
  r_init_tibble(df, n_rows);

  r_obj* desc_col = r_list_get(df, TESTS_DF_desc);
  r_obj* fn_ptr_col = r_list_get(df, TESTS_DF_fn_ptr);

  for (int i = 0; i < n_rows; ++i) {
    struct r_test test = tests[i];

    r_chr_poke(desc_col, i, r_str(test.desc));
    r_list_poke(fn_ptr_col, i, r_new_fn_ptr((r_void_fn) test.fn_ptr));
  }

  FREE(1);
  return df;
}

r_obj* ffi_run_c_test(r_obj* fn_ptr) {
  if (r_typeof(fn_ptr) != R_TYPE_pointer) {
    r_stop_unexpected_type(r_typeof(fn_ptr));
  }

  bool (*p)(void) = (bool (*)(void)) r_fn_ptr_addr(fn_ptr);
  return r_lgl(p());
}


// ------------------------------------------------------------------------

r_obj* ffi_r_string(r_obj* str) {
  return r_chr_get(str, 0);
}


// attrib.c

r_obj* r_pairlist_clone_until(r_obj* node, r_obj* sentinel, r_obj** parent_out);
r_obj* ffi_test_node_list_clone_until(r_obj* node, r_obj* sentinel) {
  r_obj* sentinel_out;
  node = KEEP(r_pairlist_clone_until(node, sentinel, &sentinel_out));

  r_obj* out = r_alloc_list(2);
  r_list_poke(out, 0, node);
  r_list_poke(out, 1, sentinel_out);

  FREE(1);
  return out;
}


// cnd.c

r_obj* ffi_test_r_warn(r_obj* x) {
  r_warn(r_chr_get_c_string(x, 0));
  return r_null;
}

r_obj* ffi_test_Rf_warning(r_obj* msg) {
  Rf_warning(r_chr_get_c_string(msg, 0));
  return r_null;
}
r_obj* ffi_test_Rf_error(r_obj* msg) {
  Rf_error(r_chr_get_c_string(msg, 0));
  return r_null;
}

r_obj* ffi_test_Rf_warningcall(r_obj* call, r_obj* msg) {
  Rf_warningcall(call, r_chr_get_c_string(msg, 0));
  return r_null;
}
r_obj* ffi_test_Rf_errorcall(r_obj* call, r_obj* msg) {
  Rf_errorcall(call, r_chr_get_c_string(msg, 0));
  return r_null;
}


// env.c

r_obj* ffi_test_base_ns_get(r_obj* name) {
  return r_base_ns_get(r_chr_get_c_string(name, 0));
}


// formula.c

extern r_obj* r_new_formula(r_obj*, r_obj*, r_obj*);


// parse.c

r_obj* ffi_test_parse(r_obj* str) {
  return r_parse(r_chr_get_c_string(str, 0));
}
r_obj* ffi_test_parse_eval(r_obj* str, r_obj* env) {
  return r_parse_eval(r_chr_get_c_string(str, 0), env);
}


// squash.c

bool rlang_is_clevel_spliceable(r_obj* x) {
  return Rf_inherits(x, "foo");
}


// stack.c

r_obj* ffi_test_sys_call(r_obj* n) {
  return r_sys_call(r_int_get(n, 0), NULL);
}
r_obj* ffi_test_sys_frame(r_obj* n) {
  return r_sys_frame(r_int_get(n, 0), NULL);
}


// vec-lgl.c

r_obj* ffi_test_lgl_sum(r_obj* x, r_obj* na_true) {
  return r_int(r_lgl_sum(x, r_lgl_get(na_true, 0)));
}
r_obj* ffi_test_lgl_which(r_obj* x, r_obj* na_true) {
  return r_lgl_which(x, r_lgl_get(na_true, 0));
}


// vec-chr.c

extern r_obj* chr_prepend(r_obj*, r_obj*);
extern r_obj* chr_append(r_obj*, r_obj*);


// internals/utils.c

r_obj* nms_are_duplicated(r_obj* nms, bool from_last);

r_obj* ffi_test_nms_are_duplicated(r_obj* nms, r_obj* from_last) {
  return nms_are_duplicated(nms, r_lgl_get(from_last, 0));
}


void rlang_init_tests(void) {
  tests_df_names = r_chr_n(tests_df_names_c_strings, TESTS_DF_SIZE);
  r_preserve_global(tests_df_names);
}

static
r_obj* tests_df_names = NULL;
