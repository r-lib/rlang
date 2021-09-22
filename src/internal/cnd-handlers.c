#include <rlang.h>

#include "decl/cnd-handlers-decl.h"

r_obj* ffi_try_catch(r_obj* try_catch_args) {
  r_obj* env = r_node_cadr(try_catch_args);

  r_obj* handlers = KEEP(rlang_env_dots_list(env));
  r_env_poke(env, rlang_syms.handlers, handlers);

  if (!r_length(handlers)) {
    FREE(1);
    return r_eval(r_syms.expr, env);
  }

  r_obj* classes = r_names(handlers);
  if (classes == r_null) {
    r_abort("`...` must be named with condition classes.");
  }

  int n = r_length(handlers);
  r_obj* const * v_classes = r_chr_cbegin(classes);

  // Build handlers arguments with updated index into the `handlers` list.
  // See `handler_call` at R level for the template.
  r_obj* args = r_null;
  r_keep_t shelter; KEEP_HERE(args, &shelter);

  for (int i = n - 1; i >= 0; --i) {
    r_obj* cls = v_classes[i];
    r_obj* hnd = KEEP(r_copy(hnd_call));

    // Equivalent to hnd[[3]][[2]][[3]][[1]][3]
    r_obj* subscript_node = r_node_cdr(r_node_cdar(r_node_cadr(r_node_cdar(r_node_cdar(r_node_cddr(hnd))))));
    r_node_poke_car(subscript_node, r_int(i + 1));

    args = r_new_node3(hnd, args, r_str_as_symbol(cls));

    KEEP_AT(args, shelter);
    FREE(1);
  }

  args = r_new_node(r_syms.expr, args);
  KEEP_AT(args, shelter);

  r_obj* call = r_new_call(rlang_syms.withCallingHandlers, args);
  KEEP_AT(call, shelter);

  r_obj* out = r_eval(call, env);

  FREE(2);
  return out;
}


void rlang_init_cnd_handlers(r_obj* ns) {
  hnd_call = r_eval(r_sym("handler_call"), ns);
  r_preserve_global(hnd_call);
}
