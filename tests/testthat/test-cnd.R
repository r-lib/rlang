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

test_that("error when msg is not a string", {
  expect_error(warn(letters), "must be a string")
})


context("restarts") # ------------------------------------------------

test_that("restarts are established", {
  with_restarts(foo = function() {}, expect_true(rst_exists("foo")))
})


context("handlers") # ------------------------------------------------

test_that("with_handlers() establishes inplace and exiting handlers", {
  handlers <- list(
    error = exiting(function(c) "caught error"),
    message = exiting(function(c) "caught message"),
    warning = calling(function(c) "warning"),
    foobar = calling(function(c) cat("foobar"))
  )

  expect_equal(with_handlers(identity(letters), splice(handlers)), identity(letters))
  expect_equal(with_handlers(stop(letters), splice(handlers)), "caught error")
  expect_equal(with_handlers(message(letters), splice(handlers)), "caught message")
  expect_warning(expect_equal(with_handlers({ warning("warn!"); letters }, splice(handlers)), identity(letters)), "warn!")
  expect_output(expect_equal(with_handlers({ signal("", "foobar"); letters }, splice(handlers)), identity(letters)), "foobar")
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
    expect_identical(class(cnd), c("foo", "bar", "rlang_condition", "condition"))
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
    expect_identical(with_handlers({ message(""); "foo" }, message = cnd_muffle), "foo")
  )
  expect_no_warning(
    expect_identical(with_handlers({ warning(""); "foo" }, warning = cnd_muffle), "foo")
  )
  cnd_expect_muffle <- calling(function(cnd) {
    expect_is(findRestart("rlang_muffle"), "restart")
    cnd_muffle(cnd)
  })
  expect_identical(with_handlers({ signal("", "cnd"); "foo" }, cnd = cnd_expect_muffle), "foo")
})

test_that("conditions have correct subclasses", {
  expect_true(inherits_all(catch_cnd(signal("", "foo")), c("foo", "rlang_condition", "condition")))
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
  skip_on_os("windows")

  f <- function() g()
  g <- function() h()
  h <- function() abort("Error message")

  # Workaround as it is not straightforward to sink stderr and
  # handle/muffle an error at the same time
  msg <- with_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env(),
    rlang__backtrace_on_error = "branch",
    conditionMessage(catch_cnd(f()))
  )

  output <- strsplit(msg, "\n")[[1]]
  expected <- readLines(test_path("test-cnd-error.txt"))
  expect_identical(!!output, expected)
})

test_that("error is printed with parent backtrace", {
  skip_on_os("windows")

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
  skip_on_os("windows")

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

test_that("signal_soft_deprecated() warns when package is attached", {
  expect_true(package_attached("utils", env()))
  expect_false(package_attached("not-attached", env()))
})

test_that("signal_soft_deprecated() warns when called from global env", {
  retired <- function(pkg, id) signal_soft_deprecated("foo", id, package = pkg)
  env_bind(global_env(), retired = retired)
  on.exit(env_unbind(global_env(), "retired"))

  with_options(lifecycle_force_verbose_retirement = FALSE, {
    locally({
      expect_no_warning(retired("not-attached", "rlang_test3"), "foo")
    })
  })

  with_options(lifecycle_force_verbose_retirement = FALSE, {
    with_env(global_env(), {
      expect_warning(retired("not-attached", "rlang_test4"), "foo")
    })
  })
})

test_that("signal_soft_deprecated() warns when option is set", {
  retired <- function(pkg, id) signal_soft_deprecated("foo", id, package = pkg)
  with_options(lifecycle_force_verbose_retirement = TRUE, {
    expect_warning(retired("utils", "rlang_test5"), "foo")
    expect_warning(retired("not-attached", "rlang_test6"), "foo")
  })
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
    rlang__backtrace_on_error = NA,
    {
      expect_warning(tryCatch(abort("foo"), error = identity), "Invalid")
      expect_null(peek_option("rlang__backtrace_on_error"))
    }
  )
})

test_that("on_error option can be tweaked", {
  skip_on_os("windows")

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
      rlang__backtrace_on_error = NULL,
      rlang_force_interactive = TRUE,
      msg()
    )

    cat_line("", "", "", ">>> Reminder:", "")
    with_options(
      rlang__backtrace_on_error = "reminder",
      rlang_force_interactive = TRUE,
      msg()
    )

    cat_line("", "", "", ">>> Reminder (not interactive):", "")
    with_options(
      rlang__backtrace_on_error = "reminder",
      rlang_force_interactive = FALSE,
      msg()
    )

    cat_line("", "", "", ">>> Branch:", "")
    with_options(rlang__backtrace_on_error = "branch", msg())

    cat_line("", "", "", ">>> Collapsed:", "")
    with_options(rlang__backtrace_on_error = "collapse", msg())
  }))
})

test_that("format_onerror_backtrace handles empty and size 1 traces", {
  scoped_options(rlang__backtrace_on_error = "branch")

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
  skip_on_os("windows")

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


# Lifecycle ----------------------------------------------------------

test_that("deprecated arguments of abort() etc still work", {
  foo <- function() {
    abort(msg = "foo", type = "bar", call = TRUE)
  }

  cnds <- catch_cnds(foo())
  msgs <- pluck_conditions_msgs(cnds)

  warnings_msgs <- msgs$warnings
  expect_length(warnings_msgs, 3L)
  expect_match(warnings_msgs[[1]], "`msg` has been renamed to `message`")
  expect_match(warnings_msgs[[2]], "`type` has been renamed to `.subclass`")
  expect_match(warnings_msgs[[3]], "`call` is deprecated")

  expect_match(msgs$error, "foo")
})

test_that("deprecated arguments of cnd_signal() still work", {
  with_non_verbose_retirement({
    observed <- catch_cnd(cnd_signal("foo"))
    expected <- catch_cnd(signal("", "foo"))
    expect_identical(observed, expected)

    with_handlers(cnd_signal(cnd("foo"), .mufflable = TRUE),
      foo = calling(function(cnd) expect_true(rst_exists("rlang_muffle")))
    )
  })
})
