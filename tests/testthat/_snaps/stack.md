# current_fn() and caller_fn() work

    Code
      (expect_error(eval_bare(quote(f(3)), env())))
    Output
      <error/rlang_error>
      Error in `frame_fn()`:
      ! `frame` must be the environment of a currently running function.

# frame_fn() returns the function of the supplied frame

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `frame_fn()`:
      ! `frame` must be the environment of a currently running function.

