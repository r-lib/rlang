# rlang and base errors are properly entraced

    Code
      cat_line(base)
    Output
      Error in h() : foo
      Calls: f -> g -> h
      Run `rlang::last_trace()` to see where the error occurred.
      <error/rlang_error>
      Error:
      ! foo
      ---
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
      <error/rlang_error>
      Error:
      ! foo
      ---
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
      Run `rlang::last_trace()` to see where the error occurred.
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
      Run rlang::last_trace(drop = FALSE) to see 1 hidden frame.
      <error/rlang_error>
      Error in `h()`:
      ! foo
      ---
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
      Run rlang::last_trace(drop = FALSE) to see 1 hidden frame.

# can supply handler environment as `bottom`

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `1 + ""`:
      ! non-numeric argument to binary operator
      ---
      Backtrace:
           x
        1. +-rlang::catch_cnd(...)
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. +-base::withCallingHandlers(...)
        9. \-rlang (local) f()
       10.   \-rlang (local) g()
       11.     \-rlang (local) h()
       12.       \-base::identity(1 + "")

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
    ---
    Backtrace:
        x
     1. \-global f()
    
    [[2]]
    <warning/rlang_warning>
    Warning in `g()`:
    bar
    ---
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
    
    
    > rlang::last_warnings(2)
    [[1]]
    <warning/rlang_warning>
    Warning in `f()`:
    foo
    ---
    Backtrace:
        x
     1. \-global f()
    
    [[2]]
    <warning/rlang_warning>
    Warning in `g()`:
    bar
    ---
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
    
    
    > summary(rlang::last_messages())
    [[1]]
    <message/rlang_message>
    Message in `message()`:
    FOO
    ---
    Backtrace:
        x
     1. \-global f()
    
    [[2]]
    <message/rlang_message>
    Message in `message()`:
    baz
    ---
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
    ---
    Backtrace:
        x
     1. \-global f()
     2.   \-global g()
     3.     \-global h()
    
    Warning message:
    In f() : foo

# can call `global_entrace()` in knitted documents

    Code
      cat_line(entrace_lines)
    Output
          options(
            rlang_backtrace_on_error_report = "full"
          )
      
          f <- function(do = stop) g(do)
          g <- function(do) h(do)
          h <- function(do) do("foo")
      
          f()
      
          ## Error in h(do): foo
      
          rlang::global_entrace()
      
          f()
      
          ## Error in `h()`:
          ## ! foo
          ## Backtrace:
          ##     x
          ##  1. \-rlang (local) f()
          ##  2.   \-rlang (local) g(do)
          ##  3.     \-rlang (local) h(do)
      
          f(warning)
      
          ## Warning in h(do): foo
      
          options(
            rlang_backtrace_on_warning_report = "full"
          )
      
          f(warning)
      
          ## Warning in h(do): foo
          ## Backtrace:
          ##     x
          ##  1. \-rlang (local) f(warning)
          ##  2.   \-rlang (local) g(do)
          ##  3.     \-rlang (local) h(do)
      
          rlang::last_warnings()
      
          ## [[1]]
          ## <warning/rlang_warning>
          ## Warning in `h()`:
          ## foo
          ## ---
          ## Backtrace:
          ##     x
          ##  1. \-rlang (local) f(warning)
          ##  2.   \-rlang (local) g(do)
          ##  3.     \-rlang (local) h(do)
          ## 
          ## [[2]]
          ## <warning/rlang_warning>
          ## Warning in `h()`:
          ## foo
          ## ---
          ## Backtrace:
          ##     x
          ##  1. \-rlang (local) f(warning)
          ##  2.   \-rlang (local) g(do)
          ##  3.     \-rlang (local) h(do)

# can't set backtrace-on-warning to reminder

    Code
      peek_backtrace_on_warning_report()
    Condition
      Warning:
      `rlang_backtrace_on_warning_report` must be one of `c("none", "branch", "full")`.
      i The option was reset to "none".
    Output
      [1] "none"

