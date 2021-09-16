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
        1. base::identity(...)
       10. rlang a()
       11. rlang b()
       12. rlang c()
       16. rlang f()
       17. rlang g()
       18. rlang h()
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
        1. +-base::identity(...)
        2. +-rlang:::catch_error(...)
        3. | \-rlang::catch_cnd(...)
        4. |   +-rlang::eval_bare(...)
        5. |   +-base::tryCatch(...)
        6. |   | \-base tryCatchList(...)
        7. |   |   \-base tryCatchOne(...)
        8. |   |     \-base doTryCatch(...)
        9. |   \-base::force(...)
       10. \-rlang a()
       11.   \-rlang b()
       12.     \-rlang c()
       13.       +-base::withCallingHandlers(...)
       14.       +-rlang::with_abort(...)
       15.       | \-base::withCallingHandlers(...)
       16.       \-rlang f()
       17.         \-rlang g()
       18.           \-rlang h()

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
       1. global f()
       2. global g()
       3. global h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      Error: foo
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
    Code
      cat_line(rlang)
    Output
      Error in `h()`: foo
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
       1. global f()
       2. global g()
       3. global h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      Error in `h()`: foo
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()

