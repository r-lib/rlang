# can use conditionMessage() method in subclasses of rlang errors

    Code
      cat_line(interactive)
    Output
      Error: dispatched!
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error: dispatched!
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
      Low-level message
      Backtrace:
        1. rlang::catch_cnd(f())
        8. rlang:::f()
        9. rlang:::g()
       10. rlang:::h()

# error is printed with parent backtrace

    Code
      print(err)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/foobar>
      Backtrace:
        1. rlang::catch_cnd(a())
        8. rlang:::a()
        9. rlang:::b()
       10. rlang:::c()
       15. rlang:::f()
       16. rlang:::g()
       17. rlang:::h()
    Code
      print(err_force)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/foobar>
        Low-level message
      Backtrace:
        1. rlang::with_options(...)
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
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/foobar>
      Backtrace:
           x
        1. +-rlang::catch_cnd(a())
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. \-rlang:::a()
        9.   \-rlang:::b()
       10.     \-rlang:::c()
       11.       +-base::tryCatch(...)
       12.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       13.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       15.       \-rlang:::f()
       16.         \-rlang:::g()
       17.           \-rlang:::h()

---

    Code
      # Full
      print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/foobar>
      Backtrace:
           x
        1. +-rlang::catch_cnd(a())
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. \-rlang:::a()
        9.   \-rlang:::b()
       10.     \-rlang:::c()
       11.       +-base::tryCatch(...)
       12.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       13.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       14.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       15.       \-rlang:::f()
       16.         \-rlang:::g()
       17.           \-rlang:::h()
    Code
      # Collapsed
      print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/foobar>
      Backtrace:
           x
        1. +-[ rlang::catch_cnd(...) ] with 6 more calls
        8. \-rlang:::a()
        9.   \-rlang:::b()
       10.     \-rlang:::c()
       11.       +-[ base::tryCatch(...) ] with 3 more calls
       15.       \-rlang:::f()
       16.         \-rlang:::g()
       17.           \-rlang:::h()
    Code
      # Branch
      print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
    Output
      x
      +-<error/rlang_error>
      | High-level message
      \-<error/foobar>
      Backtrace:
        1. rlang::catch_cnd(a())
        8. rlang:::a()
        9. rlang:::b()
       10. rlang:::c()
       15. rlang:::f()
       16. rlang:::g()
       17. rlang:::h()

# summary.rlang_error() prints full backtrace

    Code
      summary(err)
    Output
      x
      +-<error/rlang_error>
      | The high-level error message
      \-<error/rlang_error>
        The low-level error message
      Backtrace:
           x
        1. +-rlang::catch_cnd(a())
        2. | +-rlang::eval_bare(...)
        3. | +-base::tryCatch(...)
        4. | | \-base:::tryCatchList(expr, classes, parentenv, handlers)
        5. | |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
        6. | |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
        7. | \-base::force(expr)
        8. \-rlang:::a()
        9.   +-base::tryCatch(b())
       10.   | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       11.   \-rlang:::b()
       12.     \-rlang:::c()
       13.       +-base::tryCatch(f(), error = handler)
       14.       | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       15.       |   \-base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
       16.       |     \-base:::doTryCatch(return(expr), name, parentenv, handler)
       17.       \-rlang:::f()
       18.         +-base::tryCatch(g())
       19.         | \-base:::tryCatchList(expr, classes, parentenv, handlers)
       20.         \-rlang:::g()
       21.           \-rlang:::h()

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

