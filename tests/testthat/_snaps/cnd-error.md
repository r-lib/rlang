# can use conditionMessage() method in subclasses of rlang errors

    Code
      cat_line(interactive)
    Output
      Error in `h()`: dispatched!
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error in `h()`: dispatched!
      Backtrace:
          x
       1. \-global f()
       2.   \-global g()
       3.     \-global h()
      Execution halted

# rlang_error.print() calls cnd_message() methods

    Code
      print(err)
    Output
      <error/foobar>
      Error in `h()`: Low-level message
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang f()
       10. rlang g()
       11. rlang h()

# Overlapping backtraces are printed separately

    Code
      print(err)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang a()
       10. rlang b()
       11. rlang c()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang a()
       10. rlang b()
       11. rlang c()
       16. rlang f()
       17. rlang g()
       18. rlang h()

---

    Code
      print(err, simplify = "none")
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(...)
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(...)
        6. |   |   \-base tryCatchOne(...)
        7. |   |     \-base doTryCatch(...)
        8. |   \-base::force(...)
        9. \-rlang a()
       10.   \-rlang b()
       11.     \-rlang c()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(...)
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(...)
        6. |   |   \-base tryCatchOne(...)
        7. |   |     \-base doTryCatch(...)
        8. |   \-base::force(...)
        9. \-rlang a()
       10.   \-rlang b()
       11.     \-rlang c()
       12.       +-base::tryCatch(...)
       13.       | \-base tryCatchList(...)
       14.       |   \-base tryCatchOne(...)
       15.       |     \-base doTryCatch(...)
       16.       \-rlang f()
       17.         \-rlang g()
       18.           \-rlang h()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(...)
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(...)
        6. |   |   \-base tryCatchOne(...)
        7. |   |     \-base doTryCatch(...)
        8. |   \-base::force(...)
        9. \-rlang a()
       10.   \-rlang b()
       11.     \-rlang c()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(...)
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(...)
        6. |   |   \-base tryCatchOne(...)
        7. |   |     \-base doTryCatch(...)
        8. |   \-base::force(...)
        9. \-rlang a()
       10.   \-rlang b()
       11.     \-rlang c()
       12.       +-base::tryCatch(...)
       13.       | \-base tryCatchList(...)
       14.       |   \-base tryCatchOne(...)
       15.       |     \-base doTryCatch(...)
       16.       \-rlang f()
       17.         \-rlang g()
       18.           \-rlang h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
           x
        1. +-[ rlang:::catch_error(...) ] with 7 more calls
        9. \-rlang a()
       10.   \-rlang b()
       11.     \-rlang c()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
           x
        1. +-[ rlang:::catch_error(...) ] with 7 more calls
        9. \-rlang a()
       10.   \-rlang b()
       11.     \-rlang c()
       12.       +-[ base::tryCatch(...) ] with 3 more calls
       16.       \-rlang f()
       17.         \-rlang g()
       18.           \-rlang h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang a()
       10. rlang b()
       11. rlang c()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
        1. rlang:::catch_error(...)
        9. rlang a()
       10. rlang b()
       11. rlang c()
       16. rlang f()
       17. rlang g()
       18. rlang h()

# 3-level ancestry works (#1248)

    Code
      catch_error(high())
    Output
      <error/high>
      Error: 
        High-level
      Caused by error: 
        Mid-level
      Caused by error in `low()`: 
        Low-level

# summary.rlang_error() prints full backtrace

    Code
      summary(err)
    Output
      <error/rlang_error>
      Error: 
        The high-level error message
      Caused by error in `h()`: 
        The low-level error message
      Backtrace:
           x
        1. +-rlang:::catch_error(...)
        2. | \-rlang::catch_cnd(...)
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base tryCatchList(...)
        6. |   |   \-base tryCatchOne(...)
        7. |   |     \-base doTryCatch(...)
        8. |   \-base::force(...)
        9. \-rlang a()
       10.   +-base::tryCatch(...)
       11.   | \-base tryCatchList(...)
       12.   \-rlang b()
       13.     \-rlang c()
       14.       +-base::withCallingHandlers(...)
       15.       \-rlang f()
       16.         +-base::tryCatch(...)
       17.         | \-base tryCatchList(...)
       18.         \-rlang g()
       19.           \-rlang h()

# don't print message or backtrace fields if empty

    Code
      print(err)
    Output
      <error/foo>

# base parent errors are printed with rlang method

    Code
      print(rlang_err)
    Output
      <error/bar>
      Error: 
        baz
      Caused by error: 
        foo

# errors are printed with call

    Code
      print(err)
    Output
      <error/rlang_error>
      Error in `foo()`: msg

# calls are consistently displayed on rethrow (#1240)

    Code
      (expect_error(with_context(base_problem(), "step_dummy")))
    Output
      <error/rlang_error>
      Error in `step_dummy()`: 
        Problem while executing step.
      Caused by error in `base_problem()`: 
        oh no!
    Code
      (expect_error(with_context(rlang_problem(), "step_dummy")))
    Output
      <error/rlang_error>
      Error in `step_dummy()`: 
        Problem while executing step.
      Caused by error in `rlang_problem()`: 
        oh no!

# external backtraces are displayed (#1098)

    Code
      print(err)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
        1. rlang::catch_cnd(...)
        8. rlang foo()
        9. rlang bar()
       10. rlang baz()
       12. rlang f()
       13. rlang g()
       14. rlang h()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
       1. quux()
       2. foofy()
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error: 
        High-level message
      Backtrace:
           x
        1. +-rlang::catch_cnd(...)
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base tryCatchList(...)
        5. | |   \-base tryCatchOne(...)
        6. | |     \-base doTryCatch(...)
        7. | \-base::force(...)
        8. \-rlang foo()
        9.   \-rlang bar()
       10.     \-rlang baz()
       11.       +-base::withCallingHandlers(...)
       12.       \-rlang f()
       13.         \-rlang g()
       14.           \-rlang h()
      Caused by error in `h()`: 
        Low-level message
      Backtrace:
          x
       1. \-quux()
       2.   \-foofy()

# rethrowing from an exiting handler

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: 
        bar
      Backtrace:
           x
        1. +-rlang::catch_cnd(...)
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base tryCatchList(...)
        5. | |   \-base tryCatchOne(...)
        6. | |     \-base doTryCatch(...)
        7. | \-base::force(...)
        8. \-rlang foo()
        9.   \-rlang bar()
       10.     \-rlang baz()
      Caused by error in `h()`: 
        foo
      Backtrace:
           x
        1. +-rlang::catch_cnd(...)
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base tryCatchList(...)
        5. | |   \-base tryCatchOne(...)
        6. | |     \-base doTryCatch(...)
        7. | \-base::force(...)
        8. \-rlang foo()
        9.   \-rlang bar()
       10.     \-rlang baz()
       11.       +-base::tryCatch(...)
       12.       | \-base tryCatchList(...)
       13.       |   \-base tryCatchOne(...)
       14.       |     \-base doTryCatch(...)
       15.       \-rlang f()
       16.         \-rlang g()
       17.           \-rlang h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: 
        bar
      Backtrace:
           x
        1. +-[ rlang::catch_cnd(...) ] with 6 more calls
        8. \-rlang foo()
        9.   \-rlang bar()
       10.     \-rlang baz()
      Caused by error in `h()`: 
        foo
      Backtrace:
           x
        1. +-[ rlang::catch_cnd(...) ] with 6 more calls
        8. \-rlang foo()
        9.   \-rlang bar()
       10.     \-rlang baz()
       11.       +-[ base::tryCatch(...) ] with 3 more calls
       15.       \-rlang f()
       16.         \-rlang g()
       17.           \-rlang h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: 
        bar
      Backtrace:
        1. rlang::catch_cnd(...)
        8. rlang foo()
        9. rlang bar()
       10. rlang baz()
      Caused by error in `h()`: 
        foo
      Backtrace:
        1. rlang::catch_cnd(...)
        8. rlang foo()
        9. rlang bar()
       10. rlang baz()
       15. rlang f()
       16. rlang g()
       17. rlang h()

