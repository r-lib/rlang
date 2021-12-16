# Invalid on_error option resets itself

    Code
      (expect_warning(tryCatch(abort("foo"), error = identity)))
    Output
      <warning/rlang_warning>
      Warning: Invalid `rlang_backtrace_on_error` option.
      i The option was just reset to `NULL`.

# error is printed with backtrace

    Code
      cat_line(default_interactive)
    Output
      Error in `h()`:
      Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(default_non_interactive)
    Output
      Error in `h()`:
      Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-base::tryCatch(g())
       3.   | \-base tryCatchList(expr, classes, parentenv, handlers)
       4.   \-global g()
       5.     \-global h()
      Execution halted
    Code
      cat_line(reminder)
    Output
      Error in `h()`:
      Error message
      Execution halted
    Code
      cat_line(branch)
    Output
      Error in `h()`:
      Error message
      Backtrace:
       1. global f()
       4. global g()
       5. global h()
      Execution halted
    Code
      cat_line(collapse)
    Output
      Error in `h()`:
      Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-[ base::tryCatch(...) ] with 1 more call
       4.   \-global g()
       5.     \-global h()
      Execution halted
    Code
      cat_line(full)
    Output
      Error in `h()`:
      Error message
      Backtrace:
          x
       1. \-global f()
       2.   +-base::tryCatch(g())
       3.   | \-base tryCatchList(expr, classes, parentenv, handlers)
       4.   \-global g()
       5.     \-global h()
      Execution halted
    Code
      cat_line(rethrown_interactive)
    Output
      Error in `h()`:
      Error message
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(rethrown_non_interactive)
    Output
      Error in `h()`:
      Error message
      Backtrace:
          x
       1. +-base::tryCatch(f(), error = function(cnd) rlang::cnd_signal(cnd))
       2. | \-base tryCatchList(expr, classes, parentenv, handlers)
       3. |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
       4. |     \-base doTryCatch(return(expr), name, parentenv, handler)
       5. \-global f()
       6.   +-base::tryCatch(g())
       7.   | \-base tryCatchList(expr, classes, parentenv, handlers)
       8.   \-global g()
       9.     \-global h()
      Execution halted

# empty backtraces are not printed

    Code
      cat_line(branch_depth_0)
    Output
      Error: foo
      Execution halted
    Code
      cat_line(full_depth_0)
    Output
      Error: foo
      Execution halted
    Code
      cat_line(branch_depth_1)
    Output
      Error in `f()`:
      foo
      Backtrace:
       1. global f()
      Execution halted
    Code
      cat_line(full_depth_1)
    Output
      Error in `f()`:
      foo
      Backtrace:
          x
       1. \-global f()
      Execution halted

# parent errors are not displayed in error message and backtrace

    Code
      cat_line(interactive)
    Output
      Error:
        bar
      Caused by error in `h()`:
        foo
      Run `rlang::last_error()` to see where the error occurred.
      Execution halted
    Code
      cat_line(non_interactive)
    Output
      Error:
        bar
      Caused by error in `h()`:
        foo
      Backtrace:
           x
        1. \-global a()
        2.   \-global b()
        3.     \-global c()
        4.       +-base::tryCatch(...)
        5.       | \-base tryCatchList(expr, classes, parentenv, handlers)
        6.       |   \-base tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7.       |     \-base doTryCatch(return(expr), name, parentenv, handler)
        8.       \-global f()
        9.         \-global g()
       10.           \-global h()
      Execution halted

# backtrace reminder is displayed when called from `last_error()`

    Code
      # Normal case
      print(err)
    Output
      <error/rlang_error>
      Error in `h()`:
      foo
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # From `last_error()`
      print(last_error())
    Output
      <error/rlang_error>
      Error in `h()`:
      foo
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.
    Code
      # Saved from `last_error()`
      {
        saved <- last_error()
        print(saved)
      }
    Output
      <error/rlang_error>
      Error in `h()`:
      foo
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.
    Code
      # Saved from `last_error()`, but no longer last
      {
        poke_last_error(error_cnd("foo"))
        print(saved)
      }
    Output
      <error/rlang_error>
      Error in `h()`:
      foo
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
      Run `rlang::last_trace()` to see the full context.

# capture context doesn't leak into low-level backtraces

    Code
      # Non wrapped case
      {
        parent <- TRUE
        wrapper <- FALSE
        err <- catch_error(f())
        print(err)
      }
    Output
      <error/rlang_error>
      Error:
        no wrapper
      Caused by error in `failing()`:
        low-level
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # Wrapped case
      {
        wrapper <- TRUE
        err <- catch_error(f())
        print(err)
      }
    Output
      <error/rlang_error>
      Error:
        wrapper
      Caused by error in `failing()`:
        low-level
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # FIXME?
      {
        parent <- FALSE
        err <- catch_error(f())
        print(err)
      }
    Output
      <error/rlang_error>
      Error:
        wrapper
      Caused by error in `failing()`:
        low-level
      Backtrace:
        1. rlang:::catch_error(f())
        9. rlang f()
       10. rlang g()
       11. rlang h()
    Code
      # withCallingHandlers()
      print(err_wch)
    Output
      <error/rlang_error>
      Error:
        bar
      Caused by error in `baz()`:
        foo
      Backtrace:
        1. rlang:::catch_error(...)
       10. rlang foo()
       11. rlang bar(cnd)
       12. rlang baz(cnd)

# abort() displays call in error prefix

    Code
      run("rlang::abort('foo', call = quote(bar(baz)))")
    Output
      Error in `bar()`:
      foo
      Execution halted

---

    Code
      run("rlang::cnd_signal(errorCondition('foo', call = quote(bar(baz))))")
    Output
      Error in `bar()`:
      foo
      Execution halted

# abort() accepts environment as `call` field.

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `h()`:
      `arg` must be supplied.

# local_error_call() works

    Code
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `expected()`:
      tilt

# can disable error call inference for unexported functions

    Code
      (expect_error(foo()))
    Output
      <error/rlang_error>
      Error in `foo()`:
      foo
    Code
      local({
        local_options(`rlang:::restrict_default_error_call` = TRUE)
        (expect_error(foo()))
      })
    Output
      <error/rlang_error>
      Error: foo
    Code
      local({
        local_options(`rlang:::restrict_default_error_call` = TRUE)
        (expect_error(dots_list(.homonyms = "k")))
      })
    Output
      <error/rlang_error>
      Error in `dots_list()`:
      `.homonyms` must be one of "keep", "first", "last", or "error", not "k".
      i Did you mean "keep"?

# NSE doesn't interfere with error call contexts

    Code
      (expect_error(local(arg_match0("f", "foo"))))
    Output
      <error/rlang_error>
      Error: `"f"` must be one of "foo", not "f".
      i Did you mean "foo"?
    Code
      (expect_error(eval_bare(quote(arg_match0("f", "foo")))))
    Output
      <error/rlang_error>
      Error: `"f"` must be one of "foo", not "f".
      i Did you mean "foo"?
    Code
      (expect_error(eval_bare(quote(arg_match0("f", "foo")), env())))
    Output
      <error/rlang_error>
      Error: `"f"` must be one of "foo", not "f".
      i Did you mean "foo"?

# error_call() and format_error_call() preserve special syntax ops

    Code
      format_error_call(quote(1 + 2))
    Output
      [1] "`1 + 2`"

---

    Code
      format_error_call(quote(for (x in y) NULL))
    Output
      [1] "`for (x in y) NULL`"

---

    Code
      format_error_call(quote(a %||% b))
    Output
      [1] "`a %||% b`"

---

    Code
      format_error_call(quote(`%||%`()))
    Output
      [1] "``%||%`()`"

# withCallingHandlers() wrappers don't throw off trace capture on rethrow

    Code
      # `abort()` error
      print(err)
    Output
      <error/rlang_error>
      Error:
        High-level message
      Caused by error in `h()`:
        Low-level message
      Backtrace:
        1. testthat::expect_error(foo())
        7. rlang foo()
        8. rlang bar()
        9. rlang baz()
       12. rlang f()
       13. rlang g()
       14. rlang h()
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
        1. +-testthat::expect_error(foo())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang foo()
        8.   \-rlang bar()
        9.     \-rlang baz()
       10.       +-rlang wch(...)
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang f()
       13.         \-rlang g()
       14.           \-rlang h()

---

    Code
      # C-level error
      print(err)
    Output
      <error/rlang_error>
      Error:
        High-level message
      Caused by error:
        foo
      Backtrace:
        1. testthat::expect_error(foo())
        7. rlang foo()
        8. rlang bar()
        9. rlang baz()
       12. rlang f()
       13. rlang g()
       14. rlang h()
       15. rlang fail(NULL, "foo")
    Code
      summary(err)
    Output
      <error/rlang_error>
      Error:
        High-level message
      Caused by error:
        foo
      Backtrace:
           x
        1. +-testthat::expect_error(foo())
        2. | \-testthat:::expect_condition_matching(...)
        3. |   \-testthat:::quasi_capture(...)
        4. |     +-testthat .capture(...)
        5. |     | \-base::withCallingHandlers(...)
        6. |     \-rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. \-rlang foo()
        8.   \-rlang bar()
        9.     \-rlang baz()
       10.       +-rlang wch(...)
       11.       | \-base::withCallingHandlers(expr, ...)
       12.       \-rlang f()
       13.         \-rlang g()
       14.           \-rlang h()
       15.             \-rlang fail(NULL, "foo")

# `abort()` uses older bullets formatting by default

    foo
    * bar

# generic call is picked up in methods

    Code
      err(f1())
    Output
      <error/rlang_error>
      Error in `f1()`:
      foo
    Code
      err(f2())
    Output
      <error/rlang_error>
      Error in `f2()`:
      foo
    Code
      err(f3())
    Output
      <error/rlang_error>
      Error in `f3()`:
      foo
    Code
      err(f4(NULL))
    Output
      <error/rlang_error>
      Error in `f4()`:
      foo

# errors are fully displayed (parents, calls) in knitted files

    Code
      writeLines(render_md("test-parent-errors.Rmd"))
    Output
          foo <- error_cnd(
            "foo",
            message = "Parent message.",
            body = c("*" = "Bullet 1.", "*" = "Bullet 2."),
            call = call("foo"),
            use_cli_format = TRUE
          )
      
      Error.
      
          abort(
            c("Message.", "x" = "Bullet A", "i" = "Bullet B."),
            parent = foo,
            call = call("f")
          )
      
          ## Error in `f()`:
          ##   Message.
          ##   x Bullet A
          ##   i Bullet B.
          ## Caused by error in `foo()`:
          ##   Parent message.
          ##   * Bullet 1.
          ##   * Bullet 2.
      
      Warning.
      
          warn(
            c("Message.", "x" = "Bullet A", "i" = "Bullet B."),
            parent = foo,
            call = call("f")
          )
      
          ## Warning in f(): Message.
          ##   x Bullet A
          ##   i Bullet B.
          ## Caused by error in `foo()`:
          ##   Parent message.
          ##   * Bullet 1.
          ##   * Bullet 2.
      
      Message.
      
          inform(
            c("Message.", "x" = "Bullet A", "i" = "Bullet B."),
            parent = foo,
            call = call("f")
          )
      
          ## Message.
          ##   x Bullet A
          ##   i Bullet B.
          ## Caused by error in `foo()`:
          ##   Parent message.
          ##   * Bullet 1.
          ##   * Bullet 2.

# can supply bullets both through `message` and `body`

    Code
      (expect_error(abort("foo", body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error: foo
      a
      b
    Code
      (expect_error(abort(c("foo", "bar"), body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error: foo
      * bar
      a
      b

# can supply bullets both through `message` and `body` (cli case)

    Code
      (expect_error(abort("foo", body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error: foo
      a
      b
    Code
      (expect_error(abort(c("foo", "bar"), body = c("a", "b"))))
    Output
      <error/rlang_error>
      Error: foo
      bar
      a
      b

