# Invalid on_error option resets itself

    Code
      (expect_warning(tryCatch(abort("foo"), error = identity)))
    Output
      <warning/rlang_warning>
      Warning:
      Invalid `rlang_backtrace_on_error` option.
      i The option was just reset to `NULL`.

# error is printed with backtrace

    Code
      cat_line(default_interactive)
    Output
      Error in `h()`:
      ! Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(default_non_interactive)
    Output
      Error in `h()`:
      ! Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-base::tryCatch(g())
       3.   | \-base tryCatchList(expr, classes, parentenv, handlers)
       4.   \-global g()
       5.     \-global h()
       6.       \-rlang::abort("Error message")
      Execution halted
    Code
      cat_line(reminder)
    Output
      Error in `h()`:
      ! Error message
      Execution halted
    Code
      cat_line(branch)
    Output
      Error in `h()`:
      ! Error message
      Backtrace:
       1. global f()
       4. global g()
       5. global h()
      Execution halted
    Code
      cat_line(collapse)
    Output
      Error in `h()`:
      ! Error message
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
      Error in `h()`:
      ! Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-base::tryCatch(g())
       3.   | \-base tryCatchList(expr, classes, parentenv, handlers)
       4.   \-global g()
       5.     \-global h()
       6.       \-rlang::abort("Error message")
      Execution halted
    Code
      cat_line(rethrown_interactive)
    Output
      Error in `h()`:
      ! Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(rethrown_non_interactive)
    Output
      Error in `h()`:
      ! Error message
      Backtrace:
           x
        1. +-base::tryCatch(f(), error = function(cnd) rlang::cnd_signal(cnd))
        2. | \-base tryCatchList(expr, classes, parentenv, handlers)
        3. |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        4. |     \-base doTryCatch(return(expr), name, parentenv, handler)
        5. \-global f()
        6.   +-base::tryCatch(g())
        7.   | \-base tryCatchList(expr, classes, parentenv, handlers)
        8.   \-global g()
        9.     \-global h()
       10.       \-rlang::abort("Error message")
      Execution halted

# empty backtraces are not printed

    Code
      cat_line(branch_depth_0)
    Output
      Error:
      ! foo
      Backtrace:
       1. rlang::abort("foo")
      Execution halted
    Code
      cat_line(full_depth_0)
    Output
      Error:
      ! foo
      Backtrace:
          x
       1. \-rlang::abort("foo")
      Execution halted
    Code
      cat_line(branch_depth_1)
    Output
      Error in `f()`:
      ! foo
      Backtrace:
       1. global f()
      Execution halted
    Code
      cat_line(full_depth_1)
    Output
      Error in `f()`:
      ! foo
      Backtrace:
          x
       1. \-global f()
       2.   \-rlang::abort("foo")
      Execution halted

# parent errors are not displayed in error message and backtrace

    Code
      cat_line(interactive)
    Output
      Error in `c()`:
      ! bar
      Caused by error in `h()`:
      ! foo
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error in `c()`:
      ! bar
      Caused by error in `h()`:
      ! foo
      Backtrace:
           x
        1. \-global a()
        2.   \-global b()
        3.     \-global c()
        4.       +-base::tryCatch(...)
        5.       | \-base tryCatchList(expr, classes, parentenv, handlers)
        6.       |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7.       |     \-base doTryCatch(return(expr), name, parentenv, handler)
        8.       \-global f()
        9.         \-global g()
       10.           \-global h()
       11.             \-rlang::abort("foo")
      Execution halted

# backtrace reminder is displayed when called from `last_error()`

    Code
      # Normal case
      print(err)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # From `last_error()`
      print(last_error())
    Output
      <error/rlang_error>
      Error in `h()`:
      ! foo
      Backtrace:
        1. rlang:::catch_error(f())
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
      Error in `h()`:
      ! foo
      Backtrace:
        1. rlang:::catch_error(f())
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
      Error in `h()`:
      ! foo
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.

# Backtrace on rethrow: stop() - tryCatch()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)

# Backtrace on rethrow: stop() - withCallingHandlers()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::stop("low-level")
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::stop("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::stop("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::stop("low-level")

# Backtrace on rethrow: stop() - try_fetch()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::stop("low-level")
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::stop("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::stop("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::stop("low-level")

# Backtrace on rethrow: abort() - tryCatch()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       17. rlang low1()
       18. rlang low2()
       19. rlang low3()
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       17. rlang low1()
       18. rlang low2()
       19. rlang low3()
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       17. rlang low1()
       18. rlang low2()
       19. rlang low3()
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       17. rlang low1()
       18. rlang low2()
       19. rlang low3()

# Backtrace on rethrow: abort() - withCallingHandlers()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()

# Backtrace on rethrow: abort() - try_fetch()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()

# Backtrace on rethrow: warn = 2 - tryCatch()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)

# Backtrace on rethrow: warn = 2 - withCallingHandlers()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::warning("low-level")
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::warning("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::warning("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       14. rlang low1()
       15. rlang low2()
       16. rlang low3()
       17. base::warning("low-level")

# Backtrace on rethrow: warn = 2 - try_fetch()

    Code
      print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
       10. rlang high1(chain = TRUE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::warning("low-level")
    Code
      print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))
       10. rlang high1(chain = TRUE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::warning("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
       10. rlang high1(chain = FALSE, stop_helper = TRUE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::warning("low-level")
    Code
      print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
    Output
      <error/rlang_error>
      Error in `high3()`:
      ! high-level
      Caused by error in `low3()`:
      ! (converted from warning) low-level
      Backtrace:
        1. base::print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
       10. rlang high1(chain = FALSE, stop_helper = FALSE)
       11. rlang high2(...)
       12. rlang high3(...)
       19. rlang low1()
       20. rlang low2()
       21. rlang low3()
       22. base::warning("low-level")

# abort() displays call in error prefix

    Code
      run(
        "{\n      options(cli.unicode = FALSE, crayon.enabled = FALSE)\n      rlang::abort('foo', call = quote(bar(baz)))\n    }")
    Output
      Error in `bar()`:
      ! foo
      Backtrace:
          x
       1. \-rlang::abort("foo", call = quote(bar(baz)))
      Execution halted

---

    Code
      run(
        "{\n      options(cli.unicode = FALSE, crayon.enabled = FALSE)\n      rlang::cnd_signal(errorCondition('foo', call = quote(bar(baz))))\n    }")
    Output
      Error in `bar()`:
      ! foo
      Backtrace:
          x
       1. \-rlang::cnd_signal(errorCondition("foo", call = quote(bar(baz))))
      Execution halted

# abort() accepts environment as `call` field.

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `h()`:
      ! `arg` is absent but must be supplied.

# local_error_call() works

    Code
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `expected()`:
      ! tilt

# can disable error call inference for unexported functions

    Code
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! foo
    Code
      local({
        local_options(`rlang:::restrict_default_error_call` = TRUE)
        (expect_error(foo()))
      })
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! foo
    Code
      local({
        local_options(`rlang:::restrict_default_error_call` = TRUE)
        (expect_error(dots_list(.homonyms = "k")))
      })
    Output
      <error/rlang_error>
      Error in `dots_list()`:
      ! `.homonyms` must be one of "keep", "first", "last", or "error", not "k".
      i Did you mean "keep"?

# NSE doesn't interfere with error call contexts

    Code
      (expect_error(local(arg_match0("f", "foo"))))
    Output
      <error/rlang_error>
      Error:
      ! `"f"` must be one of "foo", not "f".
      i Did you mean "foo"?
    Code
      (expect_error(eval_bare(quote(arg_match0("f", "foo")))))
    Output
      <error/rlang_error>
      Error:
      ! `"f"` must be one of "foo", not "f".
      i Did you mean "foo"?
    Code
      (expect_error(eval_bare(quote(arg_match0("f", "foo")), env())))
    Output
      <error/rlang_error>
      Error:
      ! `"f"` must be one of "foo", not "f".
      i Did you mean "foo"?

# error_call() and format_error_call() preserve special syntax ops

    Code
      format_error_call(quote(1 + 2))
    Output
      [1] "`1 + 2`"

---

    Code
      format_error_call(quote(for (x in y) NULL))
    Output
      [1] "`for (x in y) NULL`"

---

    Code
      format_error_call(quote(a %||% b))
    Output
      [1] "`a %||% b`"

---

    Code
      format_error_call(quote(`%||%`()))
    Output
      [1] "``%||%`()`"

# `abort()` uses older bullets formatting by default

    foo
    * bar

# generic call is picked up in methods

    Code
      err(f1())
    Output
      <error/rlang_error>
      Error in `f1()`:
      ! foo
    Code
      err(f2())
    Output
      <error/rlang_error>
      Error in `f2()`:
      ! foo
    Code
      err(f3())
    Output
      <error/rlang_error>
      Error in `f3()`:
      ! foo
    Code
      err(f4(NULL))
    Output
      <error/rlang_error>
      Error in `f4()`:
      ! foo

# errors are fully displayed (parents, calls) in knitted files

    Code
      writeLines(render_md("test-parent-errors.Rmd"))
    Output
          foo <- error_cnd(
            "foo",
            message = "Parent message.",
            body = c("*" = "Bullet 1.", "*" = "Bullet 2."),
            call = call("foo"),
            use_cli_format = TRUE
          )
      
      Error.
      
          abort(
            c("Message.", "x" = "Bullet A", "i" = "Bullet B."),
            parent = foo,
            call = call("f")
          )
      
          ## Error in `f()`:
          ## ! Message.
          ## x Bullet A
          ## i Bullet B.
          ## Caused by error in `foo()`:
          ## ! Parent message.
          ## * Bullet 1.
          ## * Bullet 2.
      
      Warning.
      
          warn(
            c("Message.", "x" = "Bullet A", "i" = "Bullet B."),
            parent = foo,
            call = call("f")
          )
      
          ## Warning in f(): Message.
          ## x Bullet A
          ## i Bullet B.
          ## Caused by error in `foo()`:
          ## ! Parent message.
          ## * Bullet 1.
          ## * Bullet 2.
      
      Message.
      
          inform(
            c("Message.", "x" = "Bullet A", "i" = "Bullet B."),
            parent = foo,
            call = call("f")
          )
      
          ## Message.
          ## x Bullet A
          ## i Bullet B.
          ## Caused by error in `foo()`:
          ## ! Parent message.
          ## * Bullet 1.
          ## * Bullet 2.

# can supply bullets both through `message` and `body`

    Code
      (expect_error(abort("foo", body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      a
      b
    Code
      (expect_error(abort(c("foo", "bar"), body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      * bar
      a
      b

# can supply bullets both through `message` and `body` (cli case)

    Code
      (expect_error(abort("foo", body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      a
      b
    Code
      (expect_error(abort(c("foo", "bar"), body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error:
      ! foo
      bar
      a
      b

# setting `.internal` adds footer bullet

    Code
      err(abort(c("foo", x = "bar"), .internal = TRUE))
    Output
      <error/rlang_error>
      Error:
      ! foo
      x bar
      i This is an internal error, please report it to the package authors.
    Code
      err(abort("foo", body = c(x = "bar"), .internal = TRUE))
    Output
      <error/rlang_error>
      Error:
      ! foo
      x bar
      i This is an internal error, please report it to the package authors.

# setting `.internal` adds footer bullet (fallback)

    Code
      err(abort(c("foo", x = "bar"), .internal = TRUE))
    Output
      <error/rlang_error>
      Error:
      ! foo
      x bar
      i This is an internal error, please report it to the package authors.
    Code
      err(abort("foo", body = c(x = "bar"), .internal = TRUE))
    Output
      <error/rlang_error>
      Error:
      ! foo
      x bar
      i This is an internal error, please report it to the package authors.

# must pass character `body` when `message` is > 1

    Code
      err(abort("foo", body = function(cnd) c(i = "bar")))
    Output
      <error/rlang_error>
      Error:
      ! foo
      i bar
    Code
      err(abort(c("foo", "bar"), body = function() "baz"))
    Output
      <error/rlang_error>
      Error in `abort()`:
      ! Can't supply conflicting bodies in `body` and `message`.
      x `body` must be character or NULL when a length > 1 `message` is supplied.
      i `body` is currently a function.

# must pass character `body` when `message` is > 1 (non-cli case)

    Code
      err(abort("foo", body = function(cnd) c(i = "bar")))
    Output
      <error/rlang_error>
      Error:
      ! foo
      bar
    Code
      err(abort(c("foo", "bar"), body = function() "baz"))
    Output
      <error/rlang_error>
      Error in `abort()`:
      ! Can't supply conflicting bodies in `body` and `message`.
      x `body` must be character or NULL when a length > 1 `message` is supplied.
      i `body` is currently a function.

# can supply `footer`

    Code
      err(abort("foo", body = c(i = "bar"), footer = c(i = "baz")))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! foo
      i bar
      i baz
    Code
      err(abort("foo", body = function(cnd) c(i = "bar"), footer = function(cnd) c(i = "baz")))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! foo
      i bar
      i baz

# can supply `footer` (non-cli case)

    Code
      err(abort("foo", body = c(i = "bar"), footer = c(i = "baz")))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! foo
      i bar
      i baz
    Code
      err(abort("foo", body = function(cnd) c(i = "bar"), footer = function(cnd) c(i = "baz")))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! foo
      bar
      baz

# can't supply both `footer` and `.internal`

    Code
      err(abort("foo", .internal = TRUE, call = quote(f())))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! foo
      i This is an internal error, please report it to the package authors.
    Code
      err(abort("foo", footer = "bar", .internal = TRUE, call = quote(f())))
    Output
      <error/rlang_error>
      Error in `abort()`:
      ! Exactly one of `footer` or `.internal` must be supplied.

# `cli.condition_unicode_bullets` is supported by fallback formatting

    foo
    i bar

# can rethrow outside handler

    Code
      print(err(foo()))
    Output
      <error/rlang_error>
      Error in `baz()`:
      ! High-level
      Caused by error in `low()`:
      ! Low-level
      Backtrace:
        1. base::print(err(foo()))
        9. rlang foo()
       10. rlang bar()
       11. rlang baz()

