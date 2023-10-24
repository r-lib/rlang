# cnd_signal() creates a backtrace if needed

    Code
      print(err)
    Output
      <error/rlang_error_foobar>
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(f())
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. \-rlang (local) f()
        9.   \-rlang (local) g()
       10.     \-rlang (local) h()

# `inform()` and `warn()` with recurrent footer handle newlines correctly

    Code
      inform("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Message
      foo
      This message is displayed once every 8 hours.
    Code
      inform("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Message
      bar
      This message is displayed once every 8 hours.
    Code
      warn("foo", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Condition
      Warning:
      foo
      This warning is displayed once every 8 hours.
    Code
      warn("bar", .frequency = "regularly", .frequency_id = as.character(runif(1)))
    Condition
      Warning:
      bar
      This warning is displayed once every 8 hours.

# `frequency` has good error messages

    Code
      (expect_error(inform("foo", .frequency = "once", .frequency_id = NULL)))
    Output
      <error/rlang_error>
      Error in `inform()`:
      ! `.frequency_id` must be supplied with `.frequency`.
    Code
      (expect_error(warn("foo", .frequency = "once", .frequency_id = 1L)))
    Output
      <error/rlang_error>
      Error in `warn()`:
      ! `.frequency` must be a valid name, not the number 1.

# signal functions check inputs

    Code
      (expect_error(abort(error_cnd("foo"))))
    Output
      <error/rlang_error>
      Error in `abort()`:
      ! `message` must be a character vector, not a <foo> object.
    Code
      (expect_error(inform(error_cnd("foo"))))
    Output
      <error/rlang_error>
      Error in `inform()`:
      ! `message` must be a character vector, not a <foo> object.
    Code
      (expect_error(warn(class = error_cnd("foo"))))
    Output
      <error/rlang_error>
      Error in `warn()`:
      ! `class` must be a character vector, not a <foo> object.
    Code
      (expect_error(abort("foo", call = base::call)))
    Output
      <error/rlang_error>
      Error in `abort()`:
      ! `call` must be a call or environment, not a primitive function.

# error_cnd() still accepts `.subclass`

    Code
      expect_equal(error_cnd(.subclass = "foo"), error_cnd("foo"))
    Condition
      Warning:
      The `.subclass` argument of `error_cnd()` has been renamed to `class`.
    Code
      expect_error(abort("foo", .subclass = "bar"), class = "bar")
    Condition
      Warning:
      The `.subclass` argument of `abort()` has been renamed to `class`.

