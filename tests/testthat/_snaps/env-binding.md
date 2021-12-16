# env_get() without default fails

    Code
      (expect_error(env_get(env(), "foobar")))
    Output
      <error/rlang_error>
      Error in `env_get()`:
      ! Can't find `foobar` in environment.
    Code
      (expect_error(env_get_list(env(), "foobar")))
    Output
      <error/rlang_error>
      Error in `env_get_list()`:
      ! Can't find `foobar` in environment.

