#include "rlang.h"

static
SEXP base_tilde_eval(SEXP tilde, SEXP quo_env) {
  if (r_f_has_env(tilde))
    return tilde;

  static SEXP tilde_sym;
  static SEXP tilde_prim;
  if (!tilde_sym)
    tilde_sym = r_sym("~");
  if (!tilde_prim)
    tilde_prim = base_obj("~");

  // Inline the base primitive because overscopes override `~` to make
  // quosures self-evaluate
  tilde = KEEP(r_new_language_(tilde_prim, r_node_cdr(tilde)));
  tilde = KEEP(r_eval(tilde, quo_env));

  // Change it back because the result still has the primitive inlined
  r_mut_node_car(tilde, tilde_sym);

  FREE(2);
  return tilde;
}

SEXP rlang_tilde_eval(SEXP tilde, SEXP overscope, SEXP overscope_top, SEXP cur_frame) {
  if (!r_inherits(tilde, "quosure"))
    return base_tilde_eval(tilde, overscope);

  if (r_quo_is_missing(tilde))
    return(r_missing_arg());

  SEXP quo_env = r_f_env(tilde);
  SEXP prev_env = r_env_get(overscope, r_sym(".env"));
  if (r_is_null(quo_env))
    quo_env = prev_env;

  // Swap enclosures temporarily by rechaining the top of the dynamic
  // scope to the enclosure of the new formula, if it has one
  r_mut_env_parent(overscope_top, quo_env);

  SEXP exit_fun = rlang_obj("mut_env_parent");
  SEXP exit_args = r_new_pairlist2(overscope_top, prev_env);
  SEXP exit_lang = KEEP(r_new_language(exit_fun, exit_args));
  r_on_exit(exit_lang, cur_frame);
  FREE(1);

  // Update .env pronoun to current quosure env temporarily
  r_env_set(overscope, r_sym(".env"), quo_env);

  exit_fun = rlang_obj("env_set");
  exit_args = r_new_pairlist3(overscope, string(".env"), prev_env);
  exit_lang = KEEP(r_new_language(exit_fun, exit_args));
  r_on_exit(exit_lang, cur_frame);
  FREE(1);

  return r_eval(r_f_rhs(tilde), overscope);
}
