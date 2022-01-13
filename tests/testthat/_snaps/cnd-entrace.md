# rlang and base errors are properly entraced

    Code
      cat_line(base)
    Output
      Error in h() : foo
      Calls: f -> g -> h
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      Error:
      ! foo
      Backtrace:
       1. global f()
       2. global g()
       3. global h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      Error:
      ! foo
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
    Code
      cat_line(rlang)
    Output
      Error in `h()`:
      ! foo
      Run `rlang::last_error()` to see where the error occurred.
      <error/rlang_error>
      Error in `h()`:
      ! foo
      Backtrace:
       1. global f()
       2. global g()
       3. global h()
      Run `rlang::last_trace()` to see the full context.
      <error/rlang_error>
      Error in `h()`:
      ! foo
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
       4.       \-rlang::abort("foo")

# can supply handler environment as `bottom`

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `1 + ""`:
      ! non-numeric argument to binary operator
      Backtrace:
        1. rlang::catch_cnd(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()
       12. base::identity(1 + "")

# can set `entrace()` as a global handler

    Error in `1 + ""`:
    ! non-numeric argument to binary operator
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    Execution halted

---

    Error in `1 + ""`:
    ! non-numeric argument to binary operator
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
    Warning in `f()`:
    foo
    Backtrace:
     1. global f()
    
    [[2]]
    <warning/rlang_warning>
    Warning in `g()`:
    bar
    Backtrace:
     1. global f()
     2. global g()
    
    
    > rlang::last_warnings(2)
    [[1]]
    <warning/rlang_warning>
    Warning in `f()`:
    foo
    Backtrace:
     1. global f()
    
    [[2]]
    <warning/rlang_warning>
    Warning in `g()`:
    bar
    Backtrace:
     1. global f()
     2. global g()
    
    
    > summary(rlang::last_messages())
    [[1]]
    <message/rlang_message>
    Message in `message()`:
    FOO
    Backtrace:
        x
     1. \-global f()
    
    [[2]]
    <message/rlang_message>
    Message in `message()`:
    baz
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    
    
    > summary(rlang::last_messages(1))
    [[1]]
    <message/rlang_message>
    Message in `message()`:
    baz
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    
    Warning message:
    In f() : foo

