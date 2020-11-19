#include <rlang.h>


sexp* new_preserved_empty_list() {
  sexp* empty_list = r_new_vector(r_type_list, 0);
  r_mark_precious(empty_list);
  r_mark_shared(empty_list);

  sexp* nms = KEEP(r_new_vector(r_type_character, 0));
  r_poke_names(empty_list, nms);
  FREE(1);

  return empty_list;
}

void signal_soft_deprecated(const char* msg) {
  sexp* opt = r_peek_option("lifecycle_verbose_soft_deprecation");
  if (r_is_true(opt)) {
    r_warn(msg);
  }
}

sexp* eval_ext2_args(sexp* args, sexp* env) {
  // Skip native symbol object
  args = r_node_cdr(args);
  sexp* out = args;

  while (args != r_null) {
    r_node_poke_car(args, r_eval(r_node_car(args), env));
    args = r_node_cdr(args);
  }

  return out;
}

void rlang_init_utils() { }
