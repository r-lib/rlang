# withCallingHandlers() wrappers don't throw off trace capture on rethrow

    Code
      # `abort()` error
      print(err)
    Output
      <error/rlang_error>
      Error in `wch()`:
      ! High-level message
      Caused by error in `low3()`:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat (local) .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang (local) high1()
        8.   \-rlang (local) high2()
        9.     \-rlang (local) high3()
       10.       +-rlang (local) wch(low1(), error = function(err) handler1(err))
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang (local) low1()
       13.         \-rlang (local) low2()
       14.           \-rlang (local) low3()
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error in `wch()`:
      ! High-level message
      Caused by error in `low3()`:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat (local) .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang (local) high1()
        8.   \-rlang (local) high2()
        9.     \-rlang (local) high3()
       10.       +-rlang (local) wch(low1(), error = function(err) handler1(err))
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang (local) low1()
       13.         \-rlang (local) low2()
       14.           \-rlang (local) low3()

---

    Code
      # C-level error
      print(err)
    Output
      <error/rlang_error>
      Error in `wch()`:
      ! High-level message
      Caused by error:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat (local) .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang (local) high1()
        8.   \-rlang (local) high2()
        9.     \-rlang (local) high3()
       10.       +-rlang (local) wch(low1(), error = function(err) handler1(err))
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang (local) low1()
       13.         \-rlang (local) low2()
       14.           \-rlang (local) low3()
       15.             \-rlang (local) fail(NULL, "Low-level message")
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error in `wch()`:
      ! High-level message
      Caused by error:
      ! Low-level message
      ---
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat (local) .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang (local) high1()
        8.   \-rlang (local) high2()
        9.     \-rlang (local) high3()
       10.       +-rlang (local) wch(low1(), error = function(err) handler1(err))
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang (local) low1()
       13.         \-rlang (local) low2()
       14.           \-rlang (local) low3()
       15.             \-rlang (local) fail(NULL, "Low-level message")

# `parent = NA` signals a non-chained rethrow

    Code
      # Absent parent causes bad trace bottom
      hh <- (function() {
        withCallingHandlers(foo(), error = function(cnd) {
          abort(cnd_header(cnd))
        })
      })
      print(err(ff()))
    Output
      <error/rlang_error>
      Error in `h()`:
      ! bar
      ---
      Backtrace:
           x
        1. +-base::print(err(ff()))
        2. +-rlang:::err(ff())
        3. | \-testthat::expect_error(...)
        4. |   \-testthat:::expect_condition_matching(...)
        5. |     \-testthat:::quasi_capture(...)
        6. |       +-testthat (local) .capture(...)
        7. |       | \-base::withCallingHandlers(...)
        8. |       \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. +-rlang (local) ff()
       10. | \-rlang (local) gg()
       11. |   \-rlang (local) hh()
       12. |     +-base::withCallingHandlers(...)
       13. |     \-rlang (local) foo()
       14. |       \-rlang (local) bar()
       15. |         \-rlang (local) baz()
       16. |           \-base::stop("bar")
       17. \-base::.handleSimpleError(`<fn>`, "bar", quote(baz()))
       18.   \-rlang (local) h(simpleError(msg, call))
    Code
      # Missing parent allows correct trace bottom
      hh <- (function() {
        withCallingHandlers(foo(), error = function(cnd) {
          abort(cnd_header(cnd), parent = NA)
        })
      })
      print(err(ff()))
    Output
      <error/rlang_error>
      Error in `hh()`:
      ! bar
      ---
      Backtrace:
           x
        1. +-base::print(err(ff()))
        2. +-rlang:::err(ff())
        3. | \-testthat::expect_error(...)
        4. |   \-testthat:::expect_condition_matching(...)
        5. |     \-testthat:::quasi_capture(...)
        6. |       +-testthat (local) .capture(...)
        7. |       | \-base::withCallingHandlers(...)
        8. |       \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. \-rlang (local) ff()
       10.   \-rlang (local) gg()
       11.     \-rlang (local) hh()
       12.       +-base::withCallingHandlers(...)
       13.       \-rlang (local) foo()
       14.         \-rlang (local) bar()
       15.           \-rlang (local) baz()
       16.             \-base::stop("bar")
    Code
      # Wrapped handler
      handler1 <- (function(cnd, call = caller_env()) handler2(cnd, call))
      handler2 <- (function(cnd, call) abort(cnd_header(cnd), parent = NA, call = call))
      hh <- (function() {
        withCallingHandlers(foo(), error = function(cnd) handler1(cnd))
      })
      print(err(ff()))
    Output
      <error/rlang_error>
      Error in `hh()`:
      ! bar
      ---
      Backtrace:
           x
        1. +-base::print(err(ff()))
        2. +-rlang:::err(ff())
        3. | \-testthat::expect_error(...)
        4. |   \-testthat:::expect_condition_matching(...)
        5. |     \-testthat:::quasi_capture(...)
        6. |       +-testthat (local) .capture(...)
        7. |       | \-base::withCallingHandlers(...)
        8. |       \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. \-rlang (local) ff()
       10.   \-rlang (local) gg()
       11.     \-rlang (local) hh()
       12.       +-base::withCallingHandlers(foo(), error = function(cnd) handler1(cnd))
       13.       \-rlang (local) foo()
       14.         \-rlang (local) bar()
       15.           \-rlang (local) baz()
       16.             \-base::stop("bar")
    Code
      # Wrapped handler, `try_fetch()`
      hh <- (function() {
        try_fetch(foo(), error = function(cnd) handler1(cnd))
      })
      print(err(ff()))
    Output
      <error/rlang_error>
      Error in `hh()`:
      ! bar
      ---
      Backtrace:
           x
        1. +-base::print(err(ff()))
        2. +-rlang:::err(ff())
        3. | \-testthat::expect_error(...)
        4. |   \-testthat:::expect_condition_matching(...)
        5. |     \-testthat:::quasi_capture(...)
        6. |       +-testthat (local) .capture(...)
        7. |       | \-base::withCallingHandlers(...)
        8. |       \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. \-rlang (local) ff()
       10.   \-rlang (local) gg()
       11.     \-rlang (local) hh()
       12.       +-rlang::try_fetch(foo(), error = function(cnd) handler1(cnd))
       13.       | +-base::tryCatch(...)
       14.       | | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
       15.       | |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       16.       | |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
       17.       | \-base::withCallingHandlers(...)
       18.       \-rlang (local) foo()
       19.         \-rlang (local) bar()
       20.           \-rlang (local) baz()
       21.             \-base::stop("bar")
    Code
      # Wrapped handler, incorrect `call`
      hh <- (function() {
        withCallingHandlers(foo(), error = handler1)
      })
      print(err(ff()))
    Output
      <error/rlang_error>
      Error in `.handleSimpleError()`:
      ! bar
      ---
      Backtrace:
           x
        1. +-base::print(err(ff()))
        2. +-rlang:::err(ff())
        3. | \-testthat::expect_error(...)
        4. |   \-testthat:::expect_condition_matching(...)
        5. |     \-testthat:::quasi_capture(...)
        6. |       +-testthat (local) .capture(...)
        7. |       | \-base::withCallingHandlers(...)
        8. |       \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        9. \-rlang (local) ff()
       10.   \-rlang (local) gg()
       11.     \-rlang (local) hh()
       12.       +-base::withCallingHandlers(foo(), error = handler1)
       13.       \-rlang (local) foo()
       14.         \-rlang (local) bar()
       15.           \-rlang (local) baz()
       16.             \-base::stop("bar")

