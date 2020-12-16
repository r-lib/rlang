# with_abort() promotes base errors to rlang errors

    Code
      print(err)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/rlang_error>
        Low-level message
      Backtrace:
        1. base::identity(catch_cnd(a()))
        9. rlang:::a()
       10. rlang:::b()
       11. rlang:::c()
       18. rlang:::f()
       19. rlang:::g()
       20. rlang:::h()
    Code
      summary(err)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/rlang_error>
        Low-level message
      Backtrace:
           x
        1. +-base::identity(catch_cnd(a()))
        2. +-rlang::catch_cnd(a())
        3. | +-rlang::eval_bare(...)
        4. | +-base::tryCatch(...)
        5. | | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        6. | |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. | |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        8. | \-base::force(expr)
        9. \-rlang:::a()
       10.   \-rlang:::b()
       11.     \-rlang:::c()
       12.       +-base::tryCatch(...)
       13.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       14.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       15.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       16.       +-rlang::with_abort(f())
       17.       | \-base::withCallingHandlers(...)
       18.       \-rlang:::f()
       19.         \-rlang:::g()
       20.           \-rlang:::h()

# rlang and base errors are properly entraced

    Code
      cat_line(base)
    Output
      Error in h() : foo
      Calls: f -> g -> h
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      foo
      Backtrace:
       1. global::f()
       2. global::g()
       3. global::h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      foo
      Backtrace:
          █
       1. └─global::f()
       2.   └─global::g()
       3.     └─global::h()
    Code
      cat_line(rlang)
    Output
      Error: foo
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      foo
      Backtrace:
       1. global::f()
       2. global::g()
       3. global::h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      foo
      Backtrace:
          █
       1. └─global::f()
       2.   └─global::g()
       3.     └─global::h()

