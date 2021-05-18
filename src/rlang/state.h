#ifndef RLANG_STATE_H
#define RLANG_STATE_H


static inline
r_obj* r_peek_option(const char* name) {
  return Rf_GetOption1(Rf_install(name));
}

static inline
void r_poke_option(const char* name, r_obj* value) {
  r_obj* args = KEEP(r_new_node(value, r_null));
  r_node_poke_tag(args, r_sym(name));

  r_obj* call = KEEP(r_new_call(r_syms.options, args));
  r_eval(call, r_envs.base);

  FREE(2);
}


#endif
