
c_constants_env <- new.env(parent = emptyenv())

init_c_constants <- function() {
  env_bind(c_constants_env,

    tilde_thunk_fmls =
      formals(function(...) NULL),

    tilde_thunk_body = bquote(.(.Call)(.(rlang_tilde_eval),
      sys.call(),
      "data_mask_arg",
      "data_mask_top_arg",
      environment(),
      parent.frame()
    ))

  )
}
