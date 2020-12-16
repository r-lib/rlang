# cnd_signal() creates a backtrace if needed

    Code
      print(err)
    Output
      <error/rlang_error_foobar>
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()

# `inform()` and `warn()` with recurrent footer handle newlines correctly

    Code
      inform("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Message <message>
      foo
      This message is displayed once every 8 hours.
    Code
      inform("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Message <message>
      bar
      This message is displayed once every 8 hours.
    Code
      warn("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Warning <warning>
      foo
      This warning is displayed once every 8 hours.
    Code
      warn("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Warning <warning>
      bar
      This warning is displayed once every 8 hours.

