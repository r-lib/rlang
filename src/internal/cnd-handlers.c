#include <rlang.h>

#include "decl/cnd-handlers-decl.h"

r_obj* ffi_try_fetch(r_obj* try_fetch_args) {
  r_obj* env = r_node_cadr(try_fetch_args);

  r_obj* handlers = KEEP(rlang_env_dots_list(env));
  r_env_poke(env, rlang_syms.handlers, handlers);

  if (!r_length(handlers)) {
    FREE(1);
    return r_eval(r_syms.expr, env);
  }

  r_obj* classes = r_names(handlers);
  if (classes == r_null) {
    const char* arg = r_format_error_arg(r_syms.dots);
    r_abort("%s must be named with condition classes.", arg);
  }

  int n = r_length(handlers);
  r_obj* const * v_classes = r_chr_cbegin(classes);

  // Build handlers arguments with updated index into the `handlers` list.
  // See `handler_call` at R level for the template.
  r_obj* args = r_null;
  r_keep_loc shelter; KEEP_HERE(args, &shelter);

  r_obj* exiting_args = r_null;
  r_keep_loc exiting_shelter; KEEP_HERE(exiting_args, &exiting_shelter);

  for (int i = n - 1; i >= 0; --i) {
    r_obj* cls = v_classes[i];

    if (cls == r_strs.error) {
      r_obj* exiting_hnd = KEEP(r_call3(r_syms.brackets2,
                                        rlang_syms.handlers,
                                        r_int(i + 1)));
      exiting_args = r_new_node(exiting_hnd, exiting_args);
      KEEP_AT(exiting_args, exiting_shelter);

      r_node_poke_tag(exiting_args, r_syms.stack_overflow_error);
      FREE(1);
    }

    r_obj* hnd = KEEP(r_copy(hnd_call));

    // Equivalent to hnd[[3]][[3]][[3]][[1]][3]
    r_obj* subscript_node = r_node_cddr(r_node_caar(r_node_cddr(r_node_cadr(r_node_cdar(r_node_cddr(hnd))))));
    r_node_poke_car(subscript_node, r_int(i + 1));

    args = r_new_node3(hnd, args, r_str_as_symbol(cls));

    KEEP_AT(args, shelter);
    FREE(1);
  }

  args = r_new_node(r_syms.expr, args);
  KEEP_AT(args, shelter);

  r_obj* call = r_new_call(rlang_syms.withCallingHandlers, args);
  KEEP_AT(call, shelter);

  // Wrap in a `tryCatch(stackOverflowError = )` call if there are any
  // `error` handlers
  if (exiting_args != r_null) {
    exiting_args = r_new_node(call, exiting_args);
    KEEP_AT(exiting_args, exiting_shelter);

    call = r_new_call(rlang_syms.tryCatch, exiting_args);
    KEEP_AT(call, shelter);
  }

  r_obj* out = r_eval(call, env);

  FREE(3);
  return out;
}


void rlang_init_cnd_handlers(r_obj* ns) {
  hnd_call = r_eval(r_sym("handler_call"), ns);
  r_preserve_global(hnd_call);
}
