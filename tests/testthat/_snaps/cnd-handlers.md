# try_catch() checks inputs

    Code
      (expect_error(try_catch(NULL, function(...) NULL)))
    Output
      <error/rlang_error>
      Error in `try_catch()`: `...` must be named with condition classes.

# can rethrow from `try_catch()`

    Code
      err <- catch_error(try_catch(f(), error = function(cnd) abort("bar", parent = cnd)))
      print(err)
    Output
      <error/rlang_error>
      Error:
        bar
      Caused by error in `h()`:
        foo
      Backtrace:
        1. rlang:::catch_error(...)
       11. rlang f()
       12. rlang g()
       13. rlang h()
    Code
      print(err, simplify = "none")
    Output
      <error/rlang_error>
      Error:
        bar
      Caused by error in `h()`:
        foo
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
        9. +-rlang::try_catch(f(), error = function(cnd) abort("bar", parent = cnd))
       10. | \-base::withCallingHandlers(...)
       11. \-rlang f()
       12.   \-rlang g()
       13.     \-rlang h()

