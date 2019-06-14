context("cnd")

test_that("cnd() constructs all fields", {
  cond <- cnd("cnd_class", message = "cnd message")
  expect_identical(conditionMessage(cond), "cnd message")
  expect_is(cond, "cnd_class")
})

test_that("cnd() throws with unnamed fields", {
  expect_error(cnd("class", "msg", 10), "must have named data fields")
})

test_that("cnd_signal() creates muffle restarts", {
  withCallingHandlers(cnd_signal(cnd("foo")),
    foo = function(c) {
      expect_true(rst_exists("rlang_muffle"))
    }
  )
})

test_that("signallers support character vectors as `message` parameter", {
  expect_message(inform(c("foo", "bar")), "foo\nbar")
  expect_warning(warn(c("foo", "bar")), "foo\nbar")
  expect_error(abort(c("foo", "bar")), "foo\nbar")
  expect_condition(signal(c("foo", "bar"), "quux"), "quux", regex = "foo\nbar")
})


context("restarts") # ------------------------------------------------

test_that("restarts are established", {
  with_restarts(foo = function() {}, expect_true(rst_exists("foo")))
})


context("handlers") # ------------------------------------------------

test_that("with_handlers() establishes inplace and exiting handlers", {
  handlers <- list(
    error = function(c) "caught error",
    message = function(c) "caught message",
    warning = calling(function(c) "warning"),
    foobar = calling(function(c) cat("foobar"))
  )

  expect_equal(with_handlers(identity(letters), !!!handlers), identity(letters))
  expect_equal(with_handlers(stop(letters), !!!handlers), "caught error")
  expect_equal(with_handlers(message(letters), !!!handlers), "caught message")
  expect_warning(expect_equal(with_handlers({ warning("warn!"); letters }, !!!handlers), identity(letters)), "warn!")
  expect_output(expect_equal(with_handlers({ signal("", "foobar"); letters }, !!!handlers), identity(letters)), "foobar")
})

test_that("bare functions are treated as exiting handlers", {
  expect_identical(with_handlers(abort("foo"), error = function(cnd) "caught"), "caught")
})

test_that("with_handlers() supports formula shortcut for lambdas", {
  err <- with_handlers(abort("foo", "bar"), error = ~.x)
  expect_true(inherits(err, "bar"))
})

test_that("set_names2() fills in empty names", {
  chr <- c("a", b = "B", "c")
  expect_equal(set_names2(chr), c(a = "a", b = "B", c = "c"))
})

test_that("restarting() handlers pass along all requested arguments", {
  signal_foo <- function() {
    signal("", "foo", foo_field = "foo_field")
  }
  fn <- function() {
    with_handlers(signal_foo(), foo = restart_handler)
  }

  restart_handler <- restarting("rst_foo",
    a = "a",
    splice(list(b = "b")),
    .fields = c(field_arg = "foo_field")
  )

  rst_foo <- function(a, b, field_arg) {
    expect_equal(list(a, b, field_arg), list("a", "b", "foo_field"))
  }
  with_restarts(fn(), rst_foo = rst_foo)
})

test_that("cnd_signal() and signal() returns NULL invisibly", {
  expect_identical(withVisible(cnd_signal(cnd("foo"))), withVisible(invisible(NULL)))
  expect_identical(withVisible(signal("", "foo")), withVisible(invisible(NULL)))
})

test_that("signal() accepts character vectors of classes (#195)", {
  expect <- calling(function(cnd) {
    expect_identical(class(cnd), c("foo", "bar", "condition"))
  })
  with_handlers(signal("", c("foo", "bar")), foo = expect)
})

test_that("can pass condition metadata", {
  msg <- catch_cnd(inform("type", foo = "bar"))
  expect_identical(msg$foo, "bar")

  wng <- catch_cnd(warn("type", foo = "bar"))
  expect_identical(wng$foo, "bar")

  err <- catch_cnd(abort("type", foo = "bar"))
  expect_identical(err$foo, "bar")
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

test_that("can signal and catch interrupts", {
  expect_is(catch_cnd(interrupt()), "interrupt")
})

test_that("bare conditions must be subclassed", {
  expect_error(cnd(), "must be subclassed")
  expect_error(signal(""), "must be subclassed")
})

test_that("can signal interrupts with cnd_signal()", {
  intr <- catch_cnd(interrupt())
  with_handlers(cnd_signal(intr),
    condition = function(cnd) expect_is(cnd, "interrupt")
  )
})

test_that("can muffle conditions", {
  expect_no_message(
    expect_identical(with_handlers({ message(""); "foo" }, message = calling(cnd_muffle)), "foo")
  )
  expect_no_warning(
    expect_identical(with_handlers({ warning(""); "foo" }, warning = calling(cnd_muffle)), "foo")
  )
  cnd_expect_muffle <- calling(function(cnd) {
    expect_is(findRestart("rlang_muffle"), "restart")
    cnd_muffle(cnd)
  })
  expect_identical(with_handlers({ signal("", "cnd"); "foo" }, cnd = cnd_expect_muffle), "foo")
})

test_that("conditions have correct subclasses", {
  expect_true(inherits_all(catch_cnd(signal("", "foo")), c("foo", "condition", "condition")))
  expect_true(inherits_all(catch_cnd(inform("", "foo")), c("foo", "message", "condition")))
  expect_true(inherits_all(catch_cnd(warn("", "foo")), c("foo", "warning", "condition")))
  expect_true(inherits_all(catch_cnd(abort("", "foo")), c("foo", "rlang_error", "error", "condition")))
})

test_that("errors are signalled with backtrace", {
  fn <- function() abort("")
  err <- catch_cnd(fn())
  expect_is(err$trace, "rlang_trace")
})

test_that("error_cnd() checks its fields", {
  expect_no_error(error_cnd(trace = NULL))
  expect_error(error_cnd(trace = env()), "`trace` must be NULL or an rlang backtrace")
  expect_no_error(error_cnd(parent = NULL))
  expect_error(error_cnd(parent = env()), "`parent` must be NULL or a condition object")
})

test_that("error is printed with backtrace", {
  skip_unless_utf8()

  f <- function() g()
  g <- function() h()
  h <- function() abort("Error message")

  # Workaround as it is not straightforward to sink stderr and
  # handle/muffle an error at the same time
  msg <- with_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env(),
    rlang_backtrace_on_error = "branch",
    conditionMessage(catch_cnd(f()))
  )

  output <- crayon::strip_style(strsplit(msg, "\n")[[1]])
  expected <- readLines(test_path("test-cnd-error.txt"))
  expect_identical(!!output, expected)
})

test_that("error is printed with parent backtrace", {
  skip_unless_utf8()

  f <- function() g()
  g <- function() h()
  h <- function() abort("Low-level message")

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

  scoped_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )
  err <- catch_cnd(a())

  expect_known_output(file = test_path("test-cnd-error-parent-default.txt"),
    print(err)
  )
  expect_known_trace_output(err, file = "test-cnd-error-parent.txt")
})

test_that("summary.rlang_error() prints full backtrace", {
  skip_unless_utf8()

  scoped_options(
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
  c <- function() tryCatch(f(), error = handler)

  err <- catch_cnd(a())
  expect_known_output(file = test_path("test-cnd-error-str.txt"), summary(err))
})

test_that("errors are saved", {
  # `outFile` argument
  skip_if(getRversion() < "3.4")

  file <- tempfile()
  on.exit(unlink(file))

  # Verbose try() triggers conditionMessage() and thus saves the error.
  # This simulates an unhandled error.
  try(abort("foo", "bar"), outFile = file)

  expect_true(inherits_all(last_error(), c("bar", "rlang_error")))
})

test_that("can take the str() of an rlang error (#615)", {
  err <- catch_cnd(abort("foo"))
  expect_output(expect_no_error(str(err)))
})

test_that("No backtrace is displayed with top-level active bindings", {
  scoped_options(
    rlang_trace_top_env = current_env()
  )

  env_bind_active(current_env(), foo = function() abort("msg"))
  expect_error(foo, "^msg$")
})

test_that("Invalid on_error option resets itself", {
  with_options(
    rlang_backtrace_on_error = NA,
    {
      expect_warning(tryCatch(abort("foo"), error = identity), "Invalid")
      expect_null(peek_option("rlang_backtrace_on_error"))
    }
  )
})

test_that("on_error option can be tweaked", {
  skip_unless_utf8()

  f <- function() tryCatch(g())
  g <- function() h()
  h <- function() abort("The error message")
  msg <- function() cat(conditionMessage(catch_cnd(f())))

  expect_known_output(file = test_path("test-on-error-message-options.txt"), local({
    scoped_options(
      rlang_trace_top_env = current_env(),
      rlang_trace_format_srcrefs = FALSE
    )

    cat_line("", ">>> Default:", "")
    with_options(
      rlang_backtrace_on_error = NULL,
      rlang_interactive = TRUE,
      msg()
    )

    cat_line("", "", "", ">>> Reminder:", "")
    with_options(
      rlang_backtrace_on_error = "reminder",
      rlang_interactive = TRUE,
      msg()
    )

    cat_line("", "", "", ">>> Default (not interactive):", "")
    with_options(
      rlang_backtrace_on_error = NULL,
      rlang_interactive = FALSE,
      msg()
    )

    cat_line("", "", "", ">>> Branch:", "")
    with_options(rlang_backtrace_on_error = "branch", msg())

    cat_line("", "", "", ">>> Collapsed:", "")
    with_options(rlang_backtrace_on_error = "collapse", msg())
  }))
})

test_that("format_onerror_backtrace handles empty and size 1 traces", {
  scoped_options(rlang_backtrace_on_error = "branch")

  trace <- new_trace(list(), int(), chr())
  expect_identical(format_onerror_backtrace(trace), NULL)

  trace <- new_trace(list(quote(foo)), int(0), chr(""))
  expect_identical(format_onerror_backtrace(trace), NULL)

  trace <- new_trace(list(quote(foo), quote(bar)), int(0, 1), chr("", ""))
  expect_match(format_onerror_backtrace(trace), "foo.*bar")
})

test_that("don't print message or backtrace fields if empty", {
  err <- error_cnd("foo", message = "")
  expect_known_output(print(err), test_path("test-cnd-error-print-no-message.txt"))
})

test_that("with_abort() promotes base errors to rlang errors", {
  skip_unless_utf8()

  f <- function() g()
  g <- function() h()
  h <- function() stop("Low-level message")

  a <- function() b()
  b <- function() c()
  c <- function() {
    tryCatch(
      with_abort(f()),
      error = function(err) {
        abort("High-level message", parent = err)
      }
    )
  }

  scoped_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )
  err <- identity(catch_cnd(a()))

  expect_known_output(file = test_path("test-with-abort.txt"), {
    cat_line("print():", "")
    print(err)

    cat_line("", "", "summary():", "")
    summary(err)
  })
})

test_that("base parent errors are printed with rlang method", {
  base_err <- simpleError("foo")
  rlang_err <- error_cnd("bar", message = "", parent = base_err)
  expect_known_output(print(rlang_err), test_path("test-cnd-error-print-base-parent.txt"))
})

test_that("can catch condition of specific classes", {
  expect_null(catch_cnd(signal("", "bar"), "foo"))
  expect_is(catch_cnd(signal("", "bar"), "bar"), "bar")
  expect_is(catch_cnd(stop(""), "error"), "error")

  expect_is(catch_cnd(stop("tilt")), "error")
  expect_error(catch_cnd(stop("tilt"), "foo"), "tilt")

  classes <- c("foo", "bar")
  expect_is(catch_cnd(signal("", "bar"), classes), "bar")
  expect_is(catch_cnd(signal("", "foo"), classes), "foo")
})

test_that("with_abort() entraces conditions properly", {
  catch_abort <- function(signaller, arg, classes = "error") {
    f <- function() g()
    g <- function() h()
    h <- function() signaller(arg)

    catch_cnd(with_abort(f(), classes = classes))
  }

  expect_abort_trace <- function(signaller,
                                 arg,
                                 native = NULL,
                                 classes = "error") {
    err <- catch_abort(signaller, arg, classes = classes)
    expect_is(err, "rlang_error")

    trace <- err$trace
    n <- trace_length(err$trace)

    if (is_null(native)) {
      calls <- trace$calls[seq2(n - 2, n)]
      expect_true(all(
        is_call(calls[[1]], "f"),
        is_call(calls[[2]], "g"),
        is_call(calls[[3]], "h")
      ))
    } else {
      calls <- trace$calls[seq2(n - 4, n)]
      expect_true(all(
        is_call(calls[[1]], "f"),
        is_call(calls[[2]], "g"),
        is_call(calls[[3]], "h"),
        is_call(calls[[4]], "signaller"),
        is_call(calls[[5]], native)
      ))
    }
  }

  scoped_options(
    rlang_trace_top_env = current_env()
  )

  msg <- catch_abort(base::message, "")
  expect_true(inherits_all(msg, c("message", "condition")))

  err <- catch_abort(base::message, "", classes = "message")
  expect_is(err, "rlang_error")

  expect_abort_trace(base::stop, "")
  expect_abort_trace(base::stop, cnd("error"))
  expect_abort_trace(function(msg) errorcall(NULL, msg), "", "errorcall")
  expect_abort_trace(abort, "")

  expect_abort_trace(base::warning, "", classes = "warning")
  expect_abort_trace(base::warning, cnd("warning"), classes = "warning")
  expect_abort_trace(function(msg) warningcall(NULL, msg), "", "warningcall", classes = "warning")
  expect_abort_trace(warn, "", classes = "warning")

  expect_abort_trace(base::message, "", classes = "message")
  expect_abort_trace(base::message, cnd("message"), classes = "message")
  expect_abort_trace(inform, "", classes = "message")

  expect_abort_trace(base::signalCondition, cnd("foo"), classes = "condition")
})

test_that("signal context is detected", {
  get_signal_info <- function(cnd) {
    nframe <- sys.nframe() - 1
    out <- signal_context_info(nframe)
    info <- list(out[[1]], sys.call(out[[2]]))
    invokeRestart("out", info)
  }
  signal_info <- function(signaller, arg) {
    f <- function() signaller(arg)
    withRestarts(
      out = identity,
      withCallingHandlers(condition = get_signal_info, f())
    )
  }

  expect_equal(signal_info(base::stop, ""), list("stop_message", quote(f())))
  expect_equal(signal_info(base::stop, cnd("error")), list("stop_condition", quote(f())))
  expect_equal(signal_info(function(msg) errorcall(NULL, msg), ""), list("stop_native", quote(errorcall(NULL, msg))))
  expect_equal(signal_info(abort, "")[[1]], "stop_rlang")

  expect_equal(signal_info(base::warning, ""), list("warning_message", quote(f())))
  expect_equal(signal_info(base::warning, cnd("warning")), list("warning_condition", quote(f())))
  expect_equal(signal_info(function(msg) warningcall(NULL, msg), ""), list("warning_native", quote(warningcall(NULL, msg))))
  expect_equal(signal_info(warn, "")[[1]], "warning_rlang")

  expect_equal(signal_info(base::message, ""), list("message", quote(f())))
  expect_equal(signal_info(base::message, cnd("message")), list("message", quote(f())))
  expect_equal(signal_info(inform, "")[[1]], "message_rlang")

  expect_equal(signal_info(base::signalCondition, cnd("foo")), list("condition", quote(f())))

  # Warnings won't be promoted if `condition` is handled. We need to
  # handle `error` instead.
  signal_info_error <- function(signaller, arg) {
    f <- function() signaller(arg)
    withRestarts(
      out = identity,
      withCallingHandlers(error = get_signal_info, f())
    )
  }
  expr <- quote(with_options(warn = 2, signal_info_error(base::warning, "")))
  expect_equal(eval_top(expr), list("warning_promoted", quote(f())))
})

test_that("with_handlers() registers calling handlers first (#718)", {
  out <- with_restarts(
    RESTART = ~ "good",
    with_handlers(
      CND = calling(~ rst_jump("RESTART")),
      CND = ~ "bad",
      signal("", "CND")
    )
  )
  expect_identical(out, "good")

  out <- with_restarts(
    RESTART = ~ "good",
    with_handlers(
      CND = ~ "bad",
      CND = calling(~ rst_jump("RESTART")),
      signal("", "CND")
    )
  )
  expect_identical(out, "good")
})

test_that("can pass classed strings as error message", {
  message <- structure("foo", class = c("glue", "character"))
  err <- catch_cnd(abort(message))
  expect_identical(err$message, message)
})


# Lifecycle ----------------------------------------------------------

test_that("deprecated arguments of cnd_signal() still work", {
  scoped_lifecycle_silence()

  observed <- catch_cnd(cnd_signal("foo"))
  expected <- catch_cnd(signal("", "foo"))
  expect_identical(observed, expected)

  with_handlers(cnd_signal(cnd("foo"), .mufflable = TRUE),
    foo = calling(function(cnd) expect_true(rst_exists("rlang_muffle")))
  )
})
