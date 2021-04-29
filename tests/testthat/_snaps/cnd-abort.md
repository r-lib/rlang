# error is printed with backtrace

    Code
      cat_line(default_interactive)
    Output
      Error: Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(default_non_interactive)
    Output
      Error: Error message
      Backtrace:
          x
       1. \-global::f()
       2.   +-base::tryCatch(g())
       3.   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       4.   \-global::g()
       5.     \-global::h()
      Execution halted
    Code
      cat_line(reminder)
    Output
      Error: Error message
      
      Execution halted
    Code
      cat_line(branch)
    Output
      Error: Error message
      Backtrace:
       1. global::f()
       4. global::g()
       5. global::h()
      Execution halted
    Code
      cat_line(collapse)
    Output
      Error: Error message
      Backtrace:
          x
       1. \-global::f()
       2.   +-[ base::tryCatch(...) ] with 1 more call
       4.   \-global::g()
       5.     \-global::h()
      Execution halted
    Code
      cat_line(full)
    Output
      Error: Error message
      Backtrace:
          x
       1. \-global::f()
       2.   +-base::tryCatch(g())
       3.   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       4.   \-global::g()
       5.     \-global::h()
      Execution halted
    Code
      cat_line(rethrown_interactive)
    Output
      Error: Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(rethrown_non_interactive)
    Output
      Error: Error message
      Backtrace:
          x
       1. +-base::tryCatch(f(), error = function(cnd) rlang::cnd_signal(cnd))
       2. | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       3. |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       4. |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       5. \-global::f()
       6.   +-base::tryCatch(g())
       7.   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       8.   \-global::g()
       9.     \-global::h()
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
      Error: foo
      Backtrace:
       1. global::f()
      Execution halted
    Code
      cat_line(full_depth_1)
    Output
      Error: foo
      Backtrace:
          x
       1. \-global::f()
      Execution halted

# parent errors are not displayed in error message and backtrace

    Code
      cat_line(interactive)
    Output
      Error: bar
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error: bar
      Backtrace:
           x
        1. \-global::a()
        2.   \-global::b()
        3.     \-global::c()
        4.       +-base::tryCatch(...)
        5.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        6.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        8.       \-global::f()
        9.         \-global::g()
       10.           \-global::h()
      Execution halted

# backtrace reminder is displayed when called from `last_error()`

    Code
      # Normal case
      print(err)
    Output
      <error/rlang_error>
      foo
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
    Code
      # From `last_error()`
      print(last_error())
    Output
      <error/rlang_error>
      foo
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
      Run `rlang::last_trace()` to see the full context.
    Code
      # Saved from `last_error()`
      {
        saved <- last_error()
        print(saved)
      }
    Output
      <error/rlang_error>
      foo
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
      Run `rlang::last_trace()` to see the full context.
    Code
      # Saved from `last_error()`, but no longer last
      {
        last_error_env$cnd <- error_cnd("foo")
        print(saved)
      }
    Output
      <error/rlang_error>
      foo
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
      Run `rlang::last_trace()` to see the full context.

# capture context doesn't leak into low-level backtraces

    Code
      # Non wrapped case
      {
        parent <- TRUE
        wrapper <- FALSE
        err <- catch_cnd(f())
        print(err)
      }
    Output
      <error/rlang_error>
      no wrapper
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
    Code
      # Wrapped case
      {
        wrapper <- TRUE
        err <- catch_cnd(f())
        print(err)
      }
    Output
      <error/rlang_error>
      wrapper
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
    Code
      # FIXME?
      {
        parent <- FALSE
        err <- catch_cnd(f())
        print(err)
      }
    Output
      <error/rlang_error>
      wrapper
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()
    Code
      # withCallingHandlers()
      print(err_wch)
    Output
      x
      +-<error/rlang_error>
      | bar
      \-<error/rlang_error>
        foo
      Backtrace:
        1. rlang::catch_cnd(...)
        9. rlang:::foo()
       10. rlang:::bar(cnd)
       11. rlang:::baz(cnd)

