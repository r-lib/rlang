# env_is_user_facing() can be overridden

    Code
      options(rlang_user_facing = NA)
      (expect_error(env_is_user_facing(empty_env())))
    Output
      <error/rlang_error>
      Error in `env_is_user_facing()`:
      ! `options(rlang_user_facing = )` must be `TRUE`, `FALSE`, or a package name, not `NA`.
      i The option was reset to `NULL`.
    Code
      expect_null(peek_option("rlang_user_facing"))

