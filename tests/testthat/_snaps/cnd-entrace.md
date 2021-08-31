# with_abort() promotes base errors to rlang errors

    Code
      print(err)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
        1. base::identity(catch_error(a()))
       10. rlang:::a()
       11. rlang:::b()
       12. rlang:::c()
       19. rlang:::f()
       20. rlang:::g()
       21. rlang:::h()
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
        1. +-base::identity(catch_error(a()))
        2. +-rlang:::catch_error(a())
        3. | \-rlang::catch_cnd(expr, "error")
        4. |   +-rlang::eval_bare(...)
        5. |   +-base::tryCatch(...)
        6. |   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        7. |   |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        8. |   |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        9. |   \-base::force(expr)
       10. \-rlang:::a()
       11.   \-rlang:::b()
       12.     \-rlang:::c()
       13.       +-base::tryCatch(...)
       14.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       15.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       16.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       17.       +-rlang::with_abort(f())
       18.       | \-base::withCallingHandlers(...)
       19.       \-rlang:::f()
       20.         \-rlang:::g()
       21.           \-rlang:::h()

# rlang and base errors are properly entraced

    Code
      cat_line(base)
    Output
      Error in h() : foo
      Calls: f -> g -> h
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      Error: foo
      Backtrace:
       1. global::f()
       2. global::g()
       3. global::h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      Error: foo
      Backtrace:
          x
       1. \-global::f()
       2.   \-global::g()
       3.     \-global::h()
    Code
      cat_line(rlang)
    Output
      Error in `h()`: foo
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
       1. global::f()
       2. global::g()
       3. global::h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
          x
       1. \-global::f()
       2.   \-global::g()
       3.     \-global::h()

