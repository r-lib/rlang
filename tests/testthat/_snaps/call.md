# call functions type-check their input (#187)

    Code
      x <- list(a = 1)
      err(call_modify(x, NULL))
    Output
      <error/rlang_error>
      Error in `call_modify()`:
      ! `.call` must be a defused call, not a list.
    Code
      err(call_standardise(x))
    Output
      <error/rlang_error>
      Error in `call_standardise()`:
      ! `call` must be a quoted call.
    Code
      err(call_name(x))
    Output
      <error/rlang_error>
      Error in `call_name()`:
      ! `call` must be a defused call, not a list.
    Code
      err(call_args(x))
    Output
      <error/rlang_error>
      Error in `call_args()`:
      ! `call` must be a defused call, not a list.
    Code
      err(call_args_names(x))
    Output
      <error/rlang_error>
      Error in `call_args_names()`:
      ! `call` must be a defused call, not a list.
    Code
      q <- quo(!!x)
      err(call_modify(q, NULL))
    Output
      <error/rlang_error>
      Error in `call_modify()`:
      ! `.call` must be a defused call, not a list.
    Code
      err(call_standardise(q))
    Output
      <error/rlang_error>
      Error in `call_standardise()`:
      ! `call` must be a quoted call.
    Code
      err(call_name(q))
    Output
      <error/rlang_error>
      Error in `call_name()`:
      ! `call` must be a defused call, not a list.
    Code
      err(call_args(q))
    Output
      <error/rlang_error>
      Error in `call_args()`:
      ! `call` must be a defused call, not a list.
    Code
      err(call_args_names(q))
    Output
      <error/rlang_error>
      Error in `call_args_names()`:
      ! `call` must be a defused call, not a list.

