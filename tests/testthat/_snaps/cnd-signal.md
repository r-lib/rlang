# cnd_signal() creates a backtrace if needed

    Code
      print(err)
    Output
      <error/rlang_error_foobar>
      Backtrace:
        1. rlang::catch_cnd(...)
        8. rlang f()
        9. rlang g()
       10. rlang h()

# `inform()` and `warn()` with recurrent footer handle newlines correctly

    Code
      inform("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Message <rlang_message>
      foo
      This message is displayed once every 8 hours.
    Code
      inform("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Message <rlang_message>
      bar
      This message is displayed once every 8 hours.
    Code
      warn("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Warning <rlang_warning>
      foo
      This warning is displayed once every 8 hours.
    Code
      warn("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Warning <rlang_warning>
      bar
      This warning is displayed once every 8 hours.

# `frequency` has good error messages

    Code
      (expect_error(inform("foo", .frequency = "once", .frequency_id = NULL)))
    Output
      <error/rlang_error>
      Error in `inform()`: `.frequency_id` must be supplied with `.frequency`.
    Code
      (expect_error(warn("foo", .frequency = "once", .frequency_id = 1L)))
    Output
      <error/rlang_error>
      Error in `warn()`: `.frequency` must be a string.

