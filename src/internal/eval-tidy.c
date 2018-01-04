#include <rlang.h>

sexp* rlang_ns_get(const char* name);


static sexp* tilde_prim = NULL;

static sexp* base_tilde_eval(sexp* tilde, sexp* quo_env) {
  if (r_f_has_env(tilde)) {
    return tilde;
  }

  // Inline the base primitive because overscopes override `~` to make
  // quosures self-evaluate
  tilde = KEEP(r_new_call_node(tilde_prim, r_node_cdr(tilde)));
  tilde = KEEP(r_eval(tilde, quo_env));

  // Change it back because the result still has the primitive inlined
  r_node_poke_car(tilde, r_tilde_sym);

  FREE(2);
  return tilde;
}


static sexp* env_poke_parent_fn = NULL;
static sexp* env_set_fn = NULL;

sexp* rlang_tilde_eval(sexp* tilde, sexp* overscope, sexp* overscope_top, sexp* cur_frame) {
  if (!rlang_is_quosure(tilde)) {
    return base_tilde_eval(tilde, overscope);
  }
  if (quo_is_missing(tilde)) {
    return(r_missing_arg());
  }

  sexp* quo_env = r_f_env(tilde);
  sexp* prev_env = r_env_get(overscope, r_sym(".env"));
  if (r_is_null(quo_env)) {
    quo_env = prev_env;
  }

  // Swap enclosures temporarily by rechaining the top of the dynamic
  // scope to the enclosure of the new formula, if it has one
  r_env_poke_parent(overscope_top, quo_env);

  sexp* exit_args = r_build_pairlist2(overscope_top, prev_env);
  sexp* exit_lang = KEEP(r_build_call_node(env_poke_parent_fn, exit_args));
  r_on_exit(exit_lang, cur_frame);
  FREE(1);

  // Update .env pronoun to current quosure env temporarily
  r_env_poke(overscope, data_mask_env_sym, quo_env);

  exit_args = r_build_pairlist3(overscope, r_scalar_chr(".env"), prev_env);
  exit_lang = KEEP(r_build_call_node(env_set_fn, exit_args));
  r_on_exit(exit_lang, cur_frame);
  FREE(1);

  return r_eval(r_f_rhs(tilde), overscope);
}


void rlang_init_eval_tidy() {
  data_mask_sym = r_sym("_tidyeval_data_mask");
  data_mask_env_sym = r_sym(".env");
  data_mask_top_env_sym = r_sym(".top_env");

  tilde_prim = r_base_ns_get("~");
  env_poke_parent_fn = rlang_ns_get("env_poke_parent");
  env_set_fn = rlang_ns_get("env_set");
}
