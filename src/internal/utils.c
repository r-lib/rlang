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
  sexp* env = KEEP(r_current_frame());
  sexp* trace = KEEP(r_parse_eval("rlang::trace_back()", env));

  const char* source = full ?
    "print(x, simplify = 'none')" :
    "print(x, simplify = 'branch')";
  sexp* call = KEEP(r_parse(source));

  r_eval_with_x(call, r_base_env, trace);

  FREE(3);
  return;
}


void rlang_init_utils() { }
