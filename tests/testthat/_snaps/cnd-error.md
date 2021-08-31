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
       1. \-global::f()
       2.   \-global::g()
       3.     \-global::h()
      Execution halted

# rlang_error.print() calls conditionMessage() method

    Code
      print(err)
    Output
      <error/foobar>
      Error in `h()`: Low-level message
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang:::f()
       10. rlang:::g()
       11. rlang:::h()

# error is printed with parent backtrace

    Code
      print(err)
    Output
      <error/rlang_error>
      Error: High-level message
      Backtrace:
        1. rlang:::catch_error(a())
        9. rlang:::a()
       10. rlang:::b()
       11. rlang:::c()
       16. rlang:::f()
       17. rlang:::g()
       18. rlang:::h()

---

    Code
      print(err, simplify = "none")
    Output
      <error/rlang_error>
      Error: High-level message
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang:::a()
       10.   \-rlang:::b()
       11.     \-rlang:::c()
       12.       +-base::tryCatch(...)
       13.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       14.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       15.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       16.       \-rlang:::f()
       17.         \-rlang:::g()
       18.           \-rlang:::h()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: High-level message
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang:::a()
       10.   \-rlang:::b()
       11.     \-rlang:::c()
       12.       +-base::tryCatch(...)
       13.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       14.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       15.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       16.       \-rlang:::f()
       17.         \-rlang:::g()
       18.           \-rlang:::h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: High-level message
      Backtrace:
           x
        1. +-[ rlang:::catch_error(...) ] with 7 more calls
        9. \-rlang:::a()
       10.   \-rlang:::b()
       11.     \-rlang:::c()
       12.       +-[ base::tryCatch(...) ] with 3 more calls
       16.       \-rlang:::f()
       17.         \-rlang:::g()
       18.           \-rlang:::h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      <error/rlang_error>
      Error: High-level message
      Backtrace:
        1. rlang:::catch_error(a())
        9. rlang:::a()
       10. rlang:::b()
       11. rlang:::c()
       16. rlang:::f()
       17. rlang:::g()
       18. rlang:::h()

# 3-level ancestry works (#1248)

    Code
      catch_error(high())
    Output
      <error/high>
      Error: High-level
      Caused by error: Mid-level
      Caused by error in `low()`: Low-level

# summary.rlang_error() prints full backtrace

    Code
      summary(err)
    Output
      <error/rlang_error>
      Error: The high-level error message
      Caused by error in `h()`: The low-level error message
      Backtrace:
           x
        1. +-rlang:::catch_error(a())
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang:::a()
       10.   +-base::tryCatch(b())
       11.   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       12.   \-rlang:::b()
       13.     \-rlang:::c()
       14.       +-base::tryCatch(f(), error = handler)
       15.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       16.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       17.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       18.       \-rlang:::f()
       19.         +-base::tryCatch(g())
       20.         | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       21.         \-rlang:::g()
       22.           \-rlang:::h()

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
      Caused by error: foo

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
      Error in `step_dummy()`: Problem while executing step.
      Caused by error in `base_problem()`: oh no!
    Code
      (expect_error(with_context(rlang_problem(), "step_dummy")))
    Output
      <error/rlang_error>
      Error in `step_dummy()`: Problem while executing step.
      Caused by error in `rlang_problem()`: oh no!

