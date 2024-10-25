# env_get() without default fails

    Code
      env_get(env(), "foobar")
    Condition <rlang_error>
      Error in `env_get()`:
      ! Can't find `foobar` in environment.
    Code
      env_get_list(env(), "foobar")
    Condition <rlang_error>
      Error in `env_get_list()`:
      ! Can't find `foobar` in environment.

