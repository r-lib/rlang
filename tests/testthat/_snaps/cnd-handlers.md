# try_call() checks inputs

    Code
      (expect_error(try_call(NULL, function(...) NULL)))
    Output
      <error/rlang_error>
      Error in `try_call()`:
      ! `...` must be named with condition classes.

# can rethrow from `try_call()`

    Code
      err <- catch_error(try_call(f(), error = function(cnd) abort("bar", parent = cnd)))
      print(err)
    Output
      <error/rlang_error>
      Error:
      ! bar
      Caused by error in `h()`:
      ! foo
      Backtrace:
        1. rlang:::catch_error(...)
       15. rlang f()
       16. rlang g()
       17. rlang h()
    Code
      print(err, simplify = "none")
    Output
      <error/rlang_error>
      Error:
      ! bar
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. +-rlang::try_call(f(), error = function(cnd) abort("bar", parent = cnd))
       10. | +-base::tryCatch(...)
       11. | | \-base tryCatchList(expr, classes, parentenv, handlers)
       12. | |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       13. | |     \-base doTryCatch(return(expr), name, parentenv, handler)
       14. | \-base::withCallingHandlers(...)
       15. +-rlang f()
       16. | \-rlang g()
       17. |   \-rlang h()
       18. |     \-rlang::abort("foo")
       19. |       \-rlang::signal_abort(cnd, .file)
       20. |         \-base::signalCondition(cnd)
       21. \-rlang `<fn>`(`<rlng_rrr>`)
       22.   \-handlers[[1L]](cnd)
       23.     \-rlang::abort("bar", parent = cnd)
      Caused by error in `h()`:
      ! foo
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. +-rlang::try_call(f(), error = function(cnd) abort("bar", parent = cnd))
       10. | +-base::tryCatch(...)
       11. | | \-base tryCatchList(expr, classes, parentenv, handlers)
       12. | |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       13. | |     \-base doTryCatch(return(expr), name, parentenv, handler)
       14. | \-base::withCallingHandlers(...)
       15. \-rlang f()
       16.   \-rlang g()
       17.     \-rlang h()
       18.       \-rlang::abort("foo")

