context("cnd-abort")

test_that("errors are signalled with backtrace", {
  fn <- function() abort("")
  err <- catch_cnd(fn())
  expect_is(err$trace, "rlang_trace")
})

test_that("can pass classed strings as error message", {
  message <- structure("foo", class = c("glue", "character"))
  err <- catch_cnd(abort(message))
  expect_identical(err$message, message)
})

test_that("errors are saved", {
  # `outFile` argument
  skip_if(getRversion() < "3.4")

  file <- tempfile()
  on.exit(unlink(file))

  # Verbose try() triggers conditionMessage() and thus saves the error.
  # This simulates an unhandled error.
  local_options(`rlang:::force_unhandled_error` = TRUE)

  try(abort("foo", "bar"), outFile = file)
  expect_true(inherits_all(last_error(), c("bar", "rlang_error")))

  try(cnd_signal(error_cnd("foobar")), outFile = file)
  expect_true(inherits_all(last_error(), c("foobar", "rlang_error")))
})

test_that("No backtrace is displayed with top-level active bindings", {
  local_options(
    rlang_trace_top_env = current_env()
  )

  env_bind_active(current_env(), foo = function() abort("msg"))
  expect_error(foo, "^msg$")
})

test_that("Invalid on_error option resets itself", {
  with_options(
    `rlang:::force_unhandled_error` = TRUE,
    rlang_backtrace_on_error = NA,
    {
      expect_warning(tryCatch(abort("foo"), error = identity), "Invalid")
      expect_null(peek_option("rlang_backtrace_on_error"))
    }
  )
})

test_that("format_onerror_backtrace handles empty and size 1 traces", {
  local_options(rlang_backtrace_on_error = "branch")

  trace <- new_trace(list(), int(), chr())
  expect_identical(format_onerror_backtrace(trace), NULL)

  trace <- new_trace(list(quote(foo)), int(0), chr(""))
  expect_identical(format_onerror_backtrace(trace), NULL)

  trace <- new_trace(list(quote(foo), quote(bar)), int(0, 1), chr("", ""))
  expect_match(format_onerror_backtrace(error_cnd(trace = trace)), "foo.*bar")
})

test_that("error is printed with backtrace", {
  skip_unless_utf8()
  skip_if_stale_backtrace()

  run_error_script <- function(envvars = chr()) {
    run_script(test_path("fixtures", "error-backtrace.R"), envvars = envvars)
  }

  default_interactive <- run_error_script(envvars = "rlang_interactive=true")
  default_non_interactive <- run_error_script()
  reminder <- run_error_script(envvars = "rlang_backtrace_on_error=reminder")
  branch <- run_error_script(envvars = "rlang_backtrace_on_error=branch")
  collapse <- run_error_script(envvars = "rlang_backtrace_on_error=collapse")
  full <- run_error_script(envvars = "rlang_backtrace_on_error=full")

  rethrown_interactive <- run_script(
    test_path("fixtures", "error-backtrace-rethrown.R"),
    envvars = "rlang_interactive=true"
  )
  rethrown_non_interactive <- run_script(
    test_path("fixtures", "error-backtrace-rethrown.R")
  )

  verify_output(test_path("test-cnd-error.txt"), {
    "Default (interactive)"
    cat_line(default_interactive)

    "Default (non-interactive)"
    cat_line(default_non_interactive)

    "Reminder"
    cat_line(reminder)

    "Branch"
    cat_line(branch)

    "Collapse"
    cat_line(collapse)

    "Full"
    cat_line(full)

    "Rethrown (interactive)"
    cat_line(rethrown_interactive)

    "Rethrown (non-interactive)"
    cat_line(rethrown_non_interactive)
  })
})

test_that("empty backtraces are not printed", {
  skip_unless_utf8()
  skip_if_stale_backtrace()

  run_error_script <- function(envvars = chr()) {
    run_script(test_path("fixtures", "error-backtrace-empty.R"), envvars = envvars)
  }

  branch0 <- run_error_script(envvars = c("rlang_backtrace_on_error=branch", "trace_depth=0"))
  full0 <- run_error_script(envvars = c("rlang_backtrace_on_error=full", "trace_depth=0"))
  branch1 <- run_error_script(envvars = c("rlang_backtrace_on_error=branch", "trace_depth=1"))
  full1 <- run_error_script(envvars = c("rlang_backtrace_on_error=full", "trace_depth=1"))

  verify_output(test_path("test-cnd-error-empty.txt"), {
    "Branch (depth 0)"
    cat_line(branch0)

    "Full"
    cat_line(full0)

    "Branch (depth 1)"
    cat_line(branch1)

    "Full (depth 1)"
    cat_line(full1)
  })
})

test_that("parent errors are not displayed in error message and backtrace", {
  skip_unless_utf8()
  skip_if_stale_backtrace()

  run_error_script <- function(envvars = chr()) {
    run_script(
      test_path("fixtures", "error-backtrace-parent.R"),
      envvars = envvars
    )
  }
  non_interactive <- run_error_script()
  interactive <- run_error_script(envvars = "rlang_interactive=true")

  verify_output(test_path("test-cnd-error-parent.txt"), {
    "Interactive"
    cat_line(interactive)

    "Non-interactive"
    cat_line(non_interactive)
  })
})

test_that("backtrace reminder is displayed when called from `last_error()`", {
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )

  f <- function() g()
  g <- function() h()
  h <- function() abort("foo")
  err <- catch_cnd(f())

  last_error_env$cnd <- err

  verify_output(test_path("output-cnd-abort-trace-reminder.txt"), {
    "Normal case"
    print(err)

    "From `last_error()`"
    print(last_error())

    "Saved from `last_error()`"
    saved <- last_error()
    print(saved)

    "Saved from `last_error()`, but no longer last"
    last_error_env$cnd <- error_cnd("foo")
    print(saved)
  })
})

test_that("capture context doesn't leak into low-level backtraces", {
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )

  stop_wrapper <- function(...) abort("wrapper", ...)
  f <- function() g()
  g <- function() h()
  h <- function() {
    tryCatch(
      stop("low-level"),
      error = function(err) {
        if (wrapper) {
          stop_wrapper(parent = err)
        } else {
          if (parent) {
            abort("no wrapper", parent = err)
          } else {
            abort("no wrapper")
          }
        }
      }
    )
  }

  verify_output(test_path("output-cnd-abort-parent-trace.txt"), {
    wrapper <- FALSE
    err <- catch_cnd(f())
    print(err)

    wrapper <- TRUE
    err <- catch_cnd(f())
    print(err)

    "FIXME?"
    parent <- FALSE
    err <- catch_cnd(f())
    print(err)
  })
})
