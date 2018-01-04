#include <rlang.h>
#include "internal.h"


static sexp* tilde_thunk_fmls = NULL;
static sexp* tilde_thunk_body = NULL;

sexp* new_tilde_thunk(sexp* data_mask, sexp* data_mask_top) {
  sexp* body = KEEP(r_duplicate(tilde_thunk_body, false));
  sexp* fn = KEEP(r_new_function(tilde_thunk_fmls, body, r_base_env));

  sexp* args = r_node_cdr(r_node_cddr(body));
  r_node_poke_car(args, data_mask);
  r_node_poke_cadr(args, data_mask_top);

  FREE(2);
  return fn;
}


static sexp* data_mask_sym = NULL;
static sexp* data_mask_env_sym = NULL;
static sexp* data_mask_top_env_sym = NULL;

static void check_data_mask_input(sexp* env, const char* arg) {
  if (r_typeof(env) != r_type_environment) {
    r_abort("Can't create data mask because `%s` must be an environment", arg);
  }
}
sexp* rlang_new_data_mask(sexp* bottom, sexp* top, sexp* parent) {
  if (top == r_null) {
    top = bottom;
  }
  check_data_mask_input(bottom, "bottom");
  check_data_mask_input(top, "top");
  check_data_mask_input(parent, "parent");

  // Create a child because we don't know what might be in `bottom`.
  // This way we can just remove all bindings between the parent of
  // `bottom` and `top`. We don't want to clean everything in `bottom`
  // in case the environment is leaked, e.g. through a closure,
  // formula or quosure that might rely on some local bindings
  // installed by the user.
  sexp* data_mask = KEEP(r_new_environment(bottom));

  r_env_poke(data_mask, r_tilde_sym, new_tilde_thunk(data_mask, top));
  r_env_poke(data_mask, data_mask_sym, data_mask);
  r_env_poke(data_mask, data_mask_env_sym, parent);
  r_env_poke(data_mask, data_mask_top_env_sym, top);

  FREE(1);
  return data_mask;
}


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
  tilde_thunk_fmls = rlang_constants_get("tilde_thunk_fmls");
  tilde_thunk_body = rlang_constants_get("tilde_thunk_body");

  data_mask_sym = r_sym(".__tidyeval_data_mask__.");
  data_mask_env_sym = r_sym(".env");
  data_mask_top_env_sym = r_sym(".top_env");

  tilde_prim = r_base_ns_get("~");
  env_poke_parent_fn = rlang_ns_get("env_poke_parent");
  env_set_fn = rlang_ns_get("env_set");
}
