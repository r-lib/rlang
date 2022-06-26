local_unexport_signal_abort()

test_that("error_cnd() checks its fields", {
  expect_no_error(error_cnd(trace = NULL))
  expect_error(error_cnd(trace = env()), "`trace` must be `NULL` or an rlang backtrace")
  expect_no_error(error_cnd(parent = NULL))
  expect_error(error_cnd(parent = env()), "`parent` must be `NULL` or a condition object")
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

test_that("rlang_error.print() calls cnd_message() methods", {
  local_bindings(.env = global_env(),
    cnd_header.foobar = function(cnd, ...) cnd$foobar_msg
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
    cnd_header.foobar = function(c, ...) c$foobar_msg
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

test_that("cnd() constructs all fields", {
  cond <- cnd("cnd_class", message = "cnd message")
  expect_identical(conditionMessage(cond), "cnd message")
  expect_s3_class(cond, "cnd_class")
})

test_that("cnd() throws with unnamed fields", {
  expect_error(cnd("class", "msg", 10), "must have named data fields")
})

test_that("cnd_type() detects condition type", {
  expect_error(cnd_type(list()), "not a condition object")
  expect_error(cnd_type(mtcars), "not a condition object")
  expect_error(cnd_type(env()), "not a condition object")
  expect_identical(cnd_type(cnd("foo")), "condition")
  expect_identical(cnd_type(message_cnd()), "message")
  expect_identical(cnd_type(warning_cnd()), "warning")
  expect_identical(cnd_type(error_cnd()), "error")
  expect_identical(cnd_type(catch_cnd(interrupt())), "interrupt")
})

test_that("bare conditions must be subclassed", {
  expect_snapshot({
    (expect_error(cnd()))
    (expect_error(signal("")))
  })
})

test_that("predicates match condition classes", {
  expect_true(is_error(catch_cnd(stop("foo"))))
  expect_false(is_error(catch_cnd(warning("foo"))))
  expect_true(is_warning(catch_cnd(warning("foo"))))
  expect_true(is_message(catch_cnd(message("foo"))))
})

test_that("warn() and inform() signal subclassed conditions", {
  wrn <- catch_cnd(warn(""), "warning")
  msg <- catch_cnd(inform(""), "message")
  expect_equal(class(wrn), c("rlang_warning", "warning", "condition"))
  expect_equal(class(msg), c("rlang_message", "message", "condition"))
})

test_that("check for duplicate condition fields (#1268)", {
  expect_error(error_cnd("foo", foo = 1, foo = 2), "same name")
  expect_error(abort("", foo = 1, foo = 2), "same name")
})

test_that("cnd_type_header() formats condition classes", {
  expect_snapshot({
    cnd_type_header(error_cnd())
    cnd_type_header(warning_cnd())
    cnd_type_header(message_cnd())
    cnd_type_header(error_cnd(class = "foobar"))
  })
})

test_that("can format warnings and other conditions", {
  trace <- new_trace(alist(foo(), bar()), 0:1)

  warning <- warning_cnd(
    message = c("Header.", i = "Bullet."),
    call = quote(quux()),
    use_cli_format = TRUE,
    trace = trace
  )
  expect_snapshot_output(cnd_print(warning))

  message <- message_cnd(
    message = c("Header.", i = "Bullet."),
    call = quote(quux()),
    use_cli_format = TRUE,
    trace = trace,
    parent = warning
  )
  expect_snapshot_output(cnd_print(message))

  condition <- cnd(
    "foobar",
    message = c("Header.", i = "Bullet."),
    call = quote(quux()),
    use_cli_format = TRUE,
    trace = trace
  )
  expect_snapshot_output(cnd_print(condition))
})

test_that("warnings and messages have `summary()` methods", {
  warning <- warning_cnd(trace = new_trace(alist(f(), g()), 0:1))
  message <- message_cnd(trace = new_trace(alist(f(), g()), 0:1))
  expect_snapshot({
    print(warning)
    print(message)
    summary(warning)
    summary(message)
  })
})

test_that("cnd ctors check arguments", {
  expect_snapshot({
    (expect_error(warning_cnd(class = list())))
    (expect_error(error_cnd(class = list())))
    (expect_error(message_cnd(message = 1)))
  })
})

test_that("cnd_inherits() detects parent classes (#1293)", {
  expect_false(cnd_inherits(mtcars, "data.frame"))

  expect_true(cnd_inherits(cnd("foo"), "foo"))
  expect_false(cnd_inherits(cnd("foo"), "bar"))

  cnd <- cnd("foo", parent = cnd("bar"))
  expect_true(cnd_inherits(cnd, "foo"))
  expect_true(cnd_inherits(cnd, "bar"))
  expect_false(cnd_inherits(cnd, "baz"))
})

test_that("picks up cli format flag", {
  local_use_cli()
  expect_snapshot(error = TRUE, {
    cnd_signal(error_cnd(message = c("foo", "i" = "bar")))
    cnd_signal(warning_cnd(message = c("foo", "i" = "bar")))
    cnd_signal(message_cnd(message = c("foo", "i" = "bar")))
  })

  local_use_cli(format = FALSE)
  expect_snapshot(error = TRUE, {
    cnd_signal(error_cnd(message = c("foo", "i" = "bar")))
    cnd_signal(warning_cnd(message = c("foo", "i" = "bar")))
    cnd_signal(message_cnd(message = c("foo", "i" = "bar")))
  })
})

test_that("picks up caller frame", {
  get_call <- function(ctor) ctor(call = current_env())$call

  expect_equal(
    get_call(error_cnd),
    quote(get_call(error_cnd))
  )
  expect_equal(
    get_call(warning_cnd),
    quote(get_call(warning_cnd))
  )
  expect_equal(
    get_call(message_cnd),
    quote(get_call(message_cnd))
  )
  cnd2 <- function(...) cnd("foo", ...)
  expect_equal(
    get_call(cnd2),
    quote(get_call(cnd2))
  )
})

test_that("tree display option is picked up when printing errors", {
  local_options(rlang_trace_format_srcrefs = FALSE)

  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")
  cnd <- catch_cnd(f())

  expect_snapshot({
    print(cnd)

    local({
      local_options("rlang:::trace_display_tree_override" = TRUE)
      print(cnd)
    })
  })
})
