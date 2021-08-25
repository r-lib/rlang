# env_get() without default fails

    Code
      (expect_error(env_get(env(), "foobar")))
    Output
      <error/rlang_error>
      Can't find `foobar` in environment.
      Call: `env_get()`
    Code
      (expect_error(env_get_list(env(), "foobar")))
    Output
      <error/rlang_error>
      Can't find `foobar` in environment.
      Call: `env_get_list()`

