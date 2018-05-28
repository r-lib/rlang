
c_constants_env <- new.env(parent = emptyenv())

init_c_constants <- function() {
  constants <- list(
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

  nms <- names(constants)
  for (i in seq_along(constants)) {
    assign(nms[[i]], constants[[i]], envir = c_constants_env)
  }
}
