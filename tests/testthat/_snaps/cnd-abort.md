# error is printed with backtrace

    Code
      cat_line(default_interactive)
    Output
      Error in `h()`: Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(default_non_interactive)
    Output
      Error in `h()`: Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-base::tryCatch(...)
       3.   | \-base tryCatchList(...)
       4.   \-global g()
       5.     \-global h()
      Execution halted
    Code
      cat_line(reminder)
    Output
      Error in `h()`: Error message
      Execution halted
    Code
      cat_line(branch)
    Output
      Error in `h()`: Error message
      Backtrace:
       1. global f()
       4. global g()
       5. global h()
      Execution halted
    Code
      cat_line(collapse)
    Output
      Error in `h()`: Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-[ base::tryCatch(...) ] with 1 more call
       4.   \-global g()
       5.     \-global h()
      Execution halted
    Code
      cat_line(full)
    Output
      Error in `h()`: Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-base::tryCatch(...)
       3.   | \-base tryCatchList(...)
       4.   \-global g()
       5.     \-global h()
      Execution halted
    Code
      cat_line(rethrown_interactive)
    Output
      Error in `h()`: Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(rethrown_non_interactive)
    Output
      Error in `h()`: Error message
      Backtrace:
          x
       1. +-base::tryCatch(...)
       2. | \-base tryCatchList(...)
       3. |   \-base tryCatchOne(...)
       4. |     \-base doTryCatch(...)
       5. \-global f()
       6.   +-base::tryCatch(...)
       7.   | \-base tryCatchList(...)
       8.   \-global g()
       9.     \-global h()
      Execution halted

# empty backtraces are not printed

    Code
      cat_line(branch_depth_0)
    Output
      Error: foo
      Execution halted
    Code
      cat_line(full_depth_0)
    Output
      Error: foo
      Execution halted
    Code
      cat_line(branch_depth_1)
    Output
      Error in `f()`: foo
      Backtrace:
       1. global f()
      Execution halted
    Code
      cat_line(full_depth_1)
    Output
      Error in `f()`: foo
      Backtrace:
          x
       1. \-global f()
      Execution halted

# parent errors are not displayed in error message and backtrace

    Code
      cat_line(interactive)
    Output
      Error: 
        bar
      Caused by error in `h()`: 
        foo
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error: 
        bar
      Caused by error in `h()`: 
        foo
      Backtrace:
           x
        1. \-global a()
        2.   \-global b()
        3.     \-global c()
        4.       +-base::tryCatch(...)
        5.       | \-base tryCatchList(...)
        6.       |   \-base tryCatchOne(...)
        7.       |     \-base doTryCatch(...)
        8.       \-global f()
        9.         \-global g()
       10.           \-global h()
      Execution halted

# backtrace reminder is displayed when called from `last_error()`

    Code
      # Normal case
      print(err)
    Output
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # From `last_error()`
      print(last_error())
    Output
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.
    Code
      # Saved from `last_error()`
      {
        saved <- last_error()
        print(saved)
      }
    Output
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.
    Code
      # Saved from `last_error()`, but no longer last
      {
        poke_last_error(error_cnd("foo"))
        print(saved)
      }
    Output
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.

# capture context doesn't leak into low-level backtraces

    Code
      # Non wrapped case
      {
        parent <- TRUE
        wrapper <- FALSE
        err <- catch_error(f())
        print(err)
      }
    Output
      <error/rlang_error>
      Error: 
        no wrapper
      Caused by error in `failing()`: 
        low-level
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # Wrapped case
      {
        wrapper <- TRUE
        err <- catch_error(f())
        print(err)
      }
    Output
      <error/rlang_error>
      Error: 
        wrapper
      Caused by error in `failing()`: 
        low-level
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # FIXME?
      {
        parent <- FALSE
        err <- catch_error(f())
        print(err)
      }
    Output
      <error/rlang_error>
      Error: 
        wrapper
      Caused by error in `failing()`: 
        low-level
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # withCallingHandlers()
      print(err_wch)
    Output
      <error/rlang_error>
      Error: 
        bar
      Caused by error in `baz()`: 
        foo
      Backtrace:
        1. rlang:::catch_error(...)
       10. rlang foo()
       11. rlang bar(...)
       12. rlang baz(...)

# abort() displays call in error prefix

    Code
      run("rlang::abort('foo', call = quote(bar(baz)))")
    Output
      Error in `bar()`: foo
      Execution halted

---

    Code
      run("rlang::cnd_signal(errorCondition('foo', call = quote(bar(baz))))")
    Output
      Error in `bar()`: foo
      Execution halted

# abort() accepts environment as `call` field.

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `h()`: `arg` must be supplied.

# local_error_call() works

    Code
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `expected()`: tilt

# can disable error call inference for unexported functions

    Code
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `foo()`: foo
    Code
      local({
        local_options(`rlang:::restrict_default_error_call` = TRUE)
        (expect_error(foo()))
      })
    Output
      <error/rlang_error>
      Error: foo
    Code
      local({
        local_options(`rlang:::restrict_default_error_call` = TRUE)
        (expect_error(dots_list(.homonyms = "k")))
      })
    Output
      <error/rlang_error>
      Error in `dots_list()`: `.homonyms` must be one of "keep", "first", "last", or "error", not "k".
      i Did you mean "keep"?

# NSE doesn't interfere with error call contexts

    Code
      (expect_error(local(arg_match0("f", "foo"))))
    Output
      <error/rlang_error>
      Error: `f` must be one of "foo", not "f".
      i Did you mean "foo"?
    Code
      (expect_error(eval_bare(quote(arg_match0("f", "foo")))))
    Output
      <error/rlang_error>
      Error: `f` must be one of "foo", not "f".
      i Did you mean "foo"?
    Code
      (expect_error(eval_bare(quote(arg_match0("f", "foo")), env())))
    Output
      <error/rlang_error>
      Error: `f` must be one of "foo", not "f".
      i Did you mean "foo"?

# withCallingHandlers() wrappers don't throw off trace capture on rethrow

    Code
      print(err)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
        1. testthat::expect_error(...)
        7. rlang foo()
        8. rlang bar()
        9. rlang baz()
       12. rlang f()
       13. rlang g()
       14. rlang h()
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
           x
        1. +-testthat::expect_error(...)
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(...)
        7. \-rlang foo()
        8.   \-rlang bar()
        9.     \-rlang baz()
       10.       +-rlang wch(...)
       11.       | \-base::withCallingHandlers(...)
       12.       \-rlang f()
       13.         \-rlang g()
       14.           \-rlang h()

# `abort()` uses older bullets formatting by default

    foo
    * bar

