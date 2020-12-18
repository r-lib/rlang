test_that("errors are signalled with backtrace", {
  fn <- function() abort("")
  err <- catch_cnd(fn())
  expect_s3_class(err$trace, "rlang_trace")
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
  local_options(
    `rlang:::force_unhandled_error` = TRUE,
    `rlang:::error_pipe` = tempfile()
  )

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
    `rlang:::error_pipe` = tempfile(),
    rlang_backtrace_on_error = NA,
    {
      expect_warning(tryCatch(abort("foo"), error = identity), "Invalid")
      expect_null(peek_option("rlang_backtrace_on_error"))
    }
  )
})

test_that("format_onerror_backtrace handles empty and size 1 traces", {
  local_options(rlang_backtrace_on_error = "branch")

  trace <- new_trace(list(), int())
  expect_identical(format_onerror_backtrace(trace), NULL)

  trace <- new_trace(list(quote(foo)), int(0))
  expect_identical(format_onerror_backtrace(trace), NULL)

  trace <- new_trace(list(quote(foo), quote(bar)), int(0, 1))
  expect_match(format_onerror_backtrace(error_cnd(trace = trace)), "foo.*bar")
})

test_that("error is printed with backtrace", {
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

  expect_snapshot({
    cat_line(default_interactive)
    cat_line(default_non_interactive)
    cat_line(reminder)
    cat_line(branch)
    cat_line(collapse)
    cat_line(full)
    cat_line(rethrown_interactive)
    cat_line(rethrown_non_interactive)
  })
})

test_that("empty backtraces are not printed", {
  skip_if_stale_backtrace()

  run_error_script <- function(envvars = chr()) {
    run_script(test_path("fixtures", "error-backtrace-empty.R"), envvars = envvars)
  }

  branch_depth_0 <- run_error_script(envvars = c("rlang_backtrace_on_error=branch", "trace_depth=0"))
  full_depth_0 <- run_error_script(envvars = c("rlang_backtrace_on_error=full", "trace_depth=0"))
  branch_depth_1 <- run_error_script(envvars = c("rlang_backtrace_on_error=branch", "trace_depth=1"))
  full_depth_1 <- run_error_script(envvars = c("rlang_backtrace_on_error=full", "trace_depth=1"))

  expect_snapshot({
    cat_line(branch_depth_0)
    cat_line(full_depth_0)
    cat_line(branch_depth_1)
    cat_line(full_depth_1)
  })
})

test_that("parent errors are not displayed in error message and backtrace", {
  skip_if_stale_backtrace()

  run_error_script <- function(envvars = chr()) {
    run_script(
      test_path("fixtures", "error-backtrace-parent.R"),
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

  expect_snapshot({
    "Normal case"
    print(err)

    "From `last_error()`"
    print(last_error())

    "Saved from `last_error()`"
    {
      saved <- last_error()
      print(saved)
    }

    "Saved from `last_error()`, but no longer last"
    {
      last_error_env$cnd <- error_cnd("foo")
      print(saved)
    }
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

  foo <- function(cnd) bar(cnd)
  bar <- function(cnd) baz(cnd)
  baz <- function(cnd) abort("foo")
  err_wch <- catch_cnd(
    withCallingHandlers(
      foo(),
      error = function(cnd) abort("bar", parent = cnd)
    )
  )

  expect_snapshot({
    "Non wrapped case"
    {
      parent <- TRUE
      wrapper <- FALSE
      err <- catch_cnd(f())
      print(err)
    }

    "Wrapped case"
    {
      wrapper <- TRUE
      err <- catch_cnd(f())
      print(err)
    }

    "FIXME?"
    {
      parent <- FALSE
      err <- catch_cnd(f())
      print(err)
    }

    "withCallingHandlers()"
    print(err_wch)
  })
})

test_that("`.subclass` argument of `abort()` still works", {
  expect_true(inherits(catch_cnd(abort("foo", .subclass = "bar")), "bar"))
})
