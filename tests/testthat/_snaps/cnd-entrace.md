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
        1. +-base::identity(catch_error(a()))
        2. +-rlang:::catch_error(a())
        3. | \-rlang::catch_cnd(expr, "error")
        4. |   +-rlang::eval_bare(...)
        5. |   +-base::tryCatch(...)
        6. |   | \-base tryCatchList(expr, classes, parentenv, handlers)
        7. |   |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        8. |   |     \-base doTryCatch(return(expr), name, parentenv, handler)
        9. |   \-base::force(expr)
       10. \-rlang a()
       11.   \-rlang b()
       12.     \-rlang c()
       13.       +-base::withCallingHandlers(...)
       14.       +-rlang::with_abort(f())
       15.       | \-base::withCallingHandlers(expr, error = `<fn>`)
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

# can supply handler environment as `bottom`

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `+`: non-numeric argument to binary operator
      Backtrace:
        1. rlang::catch_cnd(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
       12. base::identity(1 + "")

# can set `entrace()` as a global handler

    Error in `+`: non-numeric argument to binary operator
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    Execution halted

---

    Error in `+`: non-numeric argument to binary operator
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    Execution halted

---

    FOO
    Warning in g() : bar
    baz
    > rlang::last_warnings()
    [[1]]
    <warning/rlang_warning>
    Warning in `f()`: foo
    Backtrace:
     1. global f()
    
    [[2]]
    <warning/rlang_warning>
    Warning in `g()`: bar
    Backtrace:
     1. global f()
     2. global g()
    
    
    > rlang::last_warning()
    <warning/rlang_warning>
    Warning in `g()`: bar
    Backtrace:
     1. global f()
     2. global g()
    
    > summary(rlang::last_messages())
    [[1]]
    <message/rlang_message>
    Message in `message()`: FOO
    Backtrace:
        x
     1. \-global f()
    
    [[2]]
    <message/rlang_message>
    Message in `message()`: baz
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    
    
    > summary(rlang::last_message())
    <message/rlang_message>
    Message in `message()`: baz
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    Warning message:
    In f() : foo

