
c_constants_env <- new.env(parent = emptyenv())

init_c_constants <- function() {
  env_bind(c_constants_env,

    tilde_thunk_fmls =
      formals(function(...) NULL),

    tilde_thunk_body =
      expr((!!.Call)(!!(rlang_tilde_eval),
        sys.call(),
        "data_mask_arg",
        "data_mask_top_arg",
        environment()
      ))

  )
}

# FIXME: shouldn't inline all of this at build-time! Reliance on
# .Internal() API.
rlang_current_frame <- function() NULL
body <- as.call(list(base::sys.frame, -1L))
body(rlang_current_frame) <- body
rlang_current_frame <- as.call(list(rlang_current_frame))

rlang_sys_frame <- as.call(list(base::sys.frame, 0L))
rlang_sys_call <- as.call(list(base::sys.call, 0L))
