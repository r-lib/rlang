#include <rlang.h>

r_obj* rlang_r_string(r_obj* str) {
  return r_chr_get(str, 0);
}


// attrib.c

r_obj* r_pairlist_clone_until(r_obj* node, r_obj* sentinel, r_obj** parent_out);
r_obj* rlang_test_node_list_clone_until(r_obj* node, r_obj* sentinel) {
  r_obj* sentinel_out;
  node = KEEP(r_pairlist_clone_until(node, sentinel, &sentinel_out));

  r_obj* out = r_alloc_list(2);
  r_list_poke(out, 0, node);
  r_list_poke(out, 1, sentinel_out);

  FREE(1);
  return out;
}


// cnd.c

r_obj* rlang_test_r_warn(r_obj* x) {
  r_warn(r_chr_get_c_string(x, 0));
  return r_null;
}

r_obj* rlang_test_Rf_warningcall(r_obj* call, r_obj* msg) {
  Rf_warningcall(call, r_chr_get_c_string(msg, 0));
  return r_null;
}
r_obj* rlang_test_Rf_errorcall(r_obj* call, r_obj* msg) {
  Rf_errorcall(call, r_chr_get_c_string(msg, 0));
  return r_null;
}


// env.c

r_obj* rlang_test_base_ns_get(r_obj* name) {
  return r_base_ns_get(r_chr_get_c_string(name, 0));
}


// formula.c

extern r_obj* r_new_formula(r_obj*, r_obj*, r_obj*);


// parse.c

r_obj* rlang_test_parse(r_obj* str) {
  return r_parse(r_chr_get_c_string(str, 0));
}
r_obj* rlang_test_parse_eval(r_obj* str, r_obj* env) {
  return r_parse_eval(r_chr_get_c_string(str, 0), env);
}


// squash.c

bool rlang_is_clevel_spliceable(r_obj* x) {
  return Rf_inherits(x, "foo");
}


// stack.c

r_obj* rlang_test_sys_call(r_obj* n) {
  return r_sys_call(r_int_get(n, 0), NULL);
}
r_obj* rlang_test_sys_frame(r_obj* n) {
  return r_sys_frame(r_int_get(n, 0), NULL);
}


// vec-lgl.c

r_obj* rlang_test_lgl_sum(r_obj* x, r_obj* na_true) {
  return r_int(r_lgl_sum(x, r_lgl_get(na_true, 0)));
}
r_obj* rlang_test_lgl_which(r_obj* x, r_obj* na_true) {
  return r_lgl_which(x, r_lgl_get(na_true, 0));
}


// vec-chr.c

extern r_obj* chr_prepend(r_obj*, r_obj*);
extern r_obj* chr_append(r_obj*, r_obj*);


// internals/utils.c

r_obj* nms_are_duplicated(r_obj* nms, bool from_last);

r_obj* rlang_test_nms_are_duplicated(r_obj* nms, r_obj* from_last) {
  return nms_are_duplicated(nms, r_lgl_get(from_last, 0));
}
