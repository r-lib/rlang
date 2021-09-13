test_that("error_cnd() checks its fields", {
  expect_no_error(error_cnd(trace = NULL))
  expect_error(error_cnd(trace = env()), "`trace` must be NULL or an rlang backtrace")
  expect_no_error(error_cnd(parent = NULL))
  expect_error(error_cnd(parent = env()), "`parent` must be NULL or a condition object")
})

test_that("can use conditionMessage() method in subclasses of rlang errors", {
  skip_if_stale_backtrace()

  run_error_script <- function(envvars = chr()) {
    run_script(
      test_path("fixtures", "error-backtrace-conditionMessage.R"),
      envvars = envvars
    )
  }
  non_interactive <- run_error_script()
  interactive <- run_error_script(envvars = "rlang_interactive=true")

  expect_snapshot({
    cat_line(interactive)
    cat_line(non_interactive)
  })
})

test_that("rlang_error.print() calls conditionMessage() method", {
  local_bindings(.env = global_env(),
    conditionMessage.foobar = function(c) c$foobar_msg
  )
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )

  f <- function() g()
  g <- function() h()
  h <- function() abort("", "foobar", foobar_msg = "Low-level message")

  # Handled error
  err <- catch_error(f())
  expect_snapshot(print(err))
})

# tryCatch() instead of wCH() causes distinct overlapping traces
test_that("Overlapping backtraces are printed separately", {
  # Test low-level error can use conditionMessage()
  local_bindings(.env = global_env(),
    cnd_header.foobar = function(c) c$foobar_msg
  )

  f <- function() g()
  g <- function() h()
  h <- function() abort("", "foobar", foobar_msg = "Low-level message")

  a <- function() b()
  b <- function() c()
  c <- function() {
    tryCatch(
      f(),
      error = function(err) {
        abort("High-level message", parent = err)
      }
    )
  }

  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env(),
    rlang_backtrace_on_error = "none"
  )

  err <- catch_error(a())

  expect_snapshot({
    print(err)
  })
  expect_snapshot({
    print(err, simplify = "none")
  })
  expect_snapshot_trace(err)
})

test_that("3-level ancestry works (#1248)", {
  low <- function() {
    abort("Low-level", "low")
  }
  mid <- function() {
    tryCatch(
      low(),
      error = function(err) {
        abort("Mid-level", "mid", parent = err)
      }
    )
  }
  high <- function() {
    tryCatch(
      mid(),
      error = function(err) {
        abort("High-level", "high", parent = err)
      }
    )
  }

  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env(),
    rlang_backtrace_on_error = "none"
  )

  expect_snapshot(catch_error(high()))
})

test_that("summary.rlang_error() prints full backtrace", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  f <- function() tryCatch(g())
  g <- function() h()
  h <- function() abort("The low-level error message", foo = "foo")

  handler <- function(c) {
    abort("The high-level error message", parent = c)
  }

  a <- function() tryCatch(b())
  b <- function() c()
  c <- function() withCallingHandlers(f(), error = handler)

  err <- catch_error(a())
  expect_snapshot(summary(err))
})

test_that("can take the str() of an rlang error (#615)", {
  err <- catch_error(abort("foo"))
  expect_output(expect_no_error(str(err)))
})

test_that("don't print message or backtrace fields if empty", {
  err <- error_cnd("foo", message = "")
  expect_snapshot(print(err))
})

test_that("base parent errors are printed with rlang method", {
  base_err <- simpleError("foo")
  rlang_err <- error_cnd("bar", message = "baz", parent = base_err)
  expect_snapshot(print(rlang_err))
})

test_that("errors are printed with call", {
  err <- catch_cnd(abort("msg", call = quote(foo(bar, baz))), "error")
  err$trace <- NULL
  expect_snapshot(print(err))
})

test_that("calls are consistently displayed on rethrow (#1240)", {
  base_problem <- function() stop("oh no!")
  rlang_problem <- function() abort("oh no!")

  with_context <- function(expr, step_name) {
    withCallingHandlers(
      expr = force(expr),
      error = function(cnd) {
        rlang::abort(
          message = "Problem while executing step.", 
          call = call(step_name), 
          parent = cnd
        )
      }
    )
  }

  expect_snapshot({
    (expect_error(with_context(base_problem(), "step_dummy")))
    (expect_error(with_context(rlang_problem(), "step_dummy")))
  })
})

test_that("external backtraces are displayed (#1098)", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  ext_trace <- new_trace(alist(quux(), foofy()), base::c(0L, 1L))

  f <- function() g()
  g <- function() h()
  h <- function() abort("Low-level message", trace = ext_trace)

  foo <- function() bar()
  bar <- function() baz()
  baz <- function() {
    withCallingHandlers(
      f(),
      error = function(err) {
        abort("High-level message", parent = err)
      }
    )
  }

  err <- catch_cnd(foo(), "error")

  expect_snapshot({
    print(err)
    summary(err)
  })
})

test_that("rethrowing from an exiting handler", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")

  foo <- function() bar()
  bar <- function() baz()
  baz <- function() {
    tryCatch(
      f(),
      error = function(err) abort("bar", parent = err)
    )
  }

  err <- catch_cnd(foo(), "error")
  expect_snapshot_trace(err)
})
