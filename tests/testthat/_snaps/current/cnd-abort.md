# withCallingHandlers() wrappers don't throw off trace capture on rethrow

    Code
      # `abort()` error
      print(err)
    Output
      <error/rlang_error>
      Error:
      ! High-level message
      Caused by error in `low3()`:
      ! Low-level message
      Backtrace:
        1. testthat::expect_error(high1())
        7. rlang high1()
        8. rlang high2()
        9. rlang high3()
       12. rlang low1()
       13. rlang low2()
       14. rlang low3()
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error:
      ! High-level message
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. +-rlang high1()
        8. | \-rlang high2()
        9. |   \-rlang high3()
       10. |     +-rlang wch(low1(), error = function(err) handler1(err))
       11. |     | \-base::withCallingHandlers(expr, ...)
       12. |     \-rlang low1()
       13. |       \-rlang low2()
       14. |         \-rlang low3()
       15. |           \-rlang::abort("Low-level message")
       16. |             \-rlang:::signal_abort(cnd, .file)
       17. |               \-base::signalCondition(cnd)
       18. \-rlang `<fn>`(`<rlng_rrr>`)
       19.   \-rlang handler1(err)
       20.     \-rlang handler2(err, call = call)
       21.       \-rlang::abort("High-level message", parent = err, call = call)
      Caused by error in `low3()`:
      ! Low-level message
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang high1()
        8.   \-rlang high2()
        9.     \-rlang high3()
       10.       +-rlang wch(low1(), error = function(err) handler1(err))
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang low1()
       13.         \-rlang low2()
       14.           \-rlang low3()
       15.             \-rlang::abort("Low-level message")

---

    Code
      # C-level error
      print(err)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! High-level message
      Caused by error:
      ! Low-level message
      Backtrace:
        1. testthat::expect_error(high1())
        7. rlang high1()
        8. rlang high2()
        9. rlang high3()
       12. rlang low1()
       13. rlang low2()
       14. rlang low3()
       15. rlang fail(NULL, "Low-level message")
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error in `h()`:
      ! High-level message
      Caused by error:
      ! Low-level message
      Backtrace:
           x
        1. +-testthat::expect_error(high1())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. +-rlang high1()
        8. | \-rlang high2()
        9. |   \-rlang high3()
       10. |     +-rlang wch(low1(), error = function(err) handler1(err))
       11. |     | \-base::withCallingHandlers(expr, ...)
       12. |     \-rlang low1()
       13. |       \-rlang low2()
       14. |         \-rlang low3()
       15. |           \-rlang fail(NULL, "Low-level message")
       16. \-base::.handleSimpleError(`<fn>`, "Low-level message", base::quote(NULL))
       17.   \-rlang h(simpleError(msg, call))
       18.     \-rlang handler1(err)
       19.       \-rlang handler2(err, call = call)
       20.         \-rlang::abort("High-level message", parent = err, call = call)

