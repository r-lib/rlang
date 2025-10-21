test_that("cnd_entrace() entraces conditions properly", {
  with_cnd_entrace <- function(signaller, catcher, arg, classes = "error") {
    f <- function() g()
    g <- function() h()
    h <- function() signaller(arg)

    handlers <- rep_named(
      classes,
      alist(function(cnd) {
        cnd <- cnd_entrace(cnd)
        cnd_signal(cnd)
      })
    )
    env_bind_lazy(
      current_env(),
      do = catcher(withCallingHandlers(f(), !!!handlers))
    )

    do
  }

  expect_cnd_trace <- function(
    signaller,
    catcher,
    arg,
    native = NULL,
    classes = "error",
    abort = FALSE
  ) {
    err <- with_cnd_entrace(signaller, catcher, arg, classes = classes)

    trace <- err$trace
    n <- trace_length(err$trace)

    if (is_null(trace)) {
      abort("Expected trace, got NULL.")
    }

    if (abort) {
      calls <- trace$call[seq2(n - 3, n)]
      expect_true(all(
        is_call(calls[[1]], "f"),
        is_call(calls[[2]], "g"),
        is_call(calls[[3]], "h"),
        is_call(calls[[4]], "signaller")
      ))
    } else if (is_null(native)) {
      calls <- trace$call[seq2(n - 2, n)]
      expect_true(all(
        is_call(calls[[1]], "f"),
        is_call(calls[[2]], "g"),
        is_call(calls[[3]], "h")
      ))
    } else {
      calls <- trace$call[seq2(n - 4, n)]
      expect_true(all(
        is_call(calls[[1]], "f"),
        is_call(calls[[2]], "g"),
        is_call(calls[[3]], "h"),
        is_call(calls[[4]], "signaller"),
        is_call(calls[[5]], native)
      ))
    }
  }

  local_options(
    rlang_trace_top_env = current_env()
  )

  with_cnd_entrace(base::message, catch_message, "")
  with_cnd_entrace(base::message, catch_error, "", classes = "message")

  expect_cnd_trace(base::stop, catch_error, "")
  expect_cnd_trace(base::stop, catch_error, cnd("error"))
  expect_cnd_trace(
    function(msg) errorcall(NULL, msg),
    catch_error,
    "",
    "errorcall"
  )
  expect_cnd_trace(abort, catch_error, "", abort = TRUE)

  expect_cnd_trace(base::warning, catch_warning, "", classes = "warning")
  expect_cnd_trace(
    base::warning,
    catch_warning,
    cnd("warning"),
    classes = "warning"
  )
  expect_cnd_trace(
    function(msg) warningcall(NULL, msg),
    catch_warning,
    "",
    "warningcall",
    classes = "warning"
  )
  expect_cnd_trace(warn, catch_warning, "", classes = "warning")

  expect_cnd_trace(base::message, catch_message, "", classes = "message")
  expect_cnd_trace(
    base::message,
    catch_message,
    cnd("message"),
    classes = "message"
  )
  expect_cnd_trace(inform, catch_message, "", classes = "message")

  expect_cnd_trace(
    base::signalCondition,
    catch_cnd,
    cnd("foo"),
    classes = "condition"
  )
})

test_that("signal context is detected", {
  get_signal_info <- function(cnd) {
    nframe <- sys.nframe() - 1
    out <- signal_context_info(nframe)
    info <- list(out[[1]], sys.call(out[[2]]))
    invokeRestart("out", info)
  }
  signal_info <- function(class, signaller, arg) {
    f <- function() signaller(arg)
    hnd <- set_names(list(get_signal_info), class)
    inject(
      withRestarts(
        out = identity,
        withCallingHandlers(!!!hnd, f())
      )
    )
  }

  expect_equal(
    signal_info("error", base::stop, ""),
    list("stop_message", quote(f()))
  )
  expect_equal(
    signal_info("error", base::stop, cnd("error")),
    list("stop_condition", quote(f()))
  )
  expect_equal(
    signal_info("error", function(msg) errorcall(NULL, msg), ""),
    list("stop_native", quote(errorcall(NULL, msg)))
  )

  # No longer works since we switched to signalCondition approach
  # expect_equal(signal_info(abort, "")[[1]], "stop_rlang")

  expect_equal(
    signal_info("warning", base::warning, ""),
    list("warning_message", quote(f()))
  )
  expect_equal(
    signal_info("warning", base::warning, cnd("warning")),
    list("warning_condition", quote(f()))
  )
  expect_equal(
    signal_info("warning", function(msg) warningcall(NULL, msg), ""),
    list("warning_native", quote(warningcall(NULL, msg)))
  )
  expect_equal(signal_info("warning", warn, "")[[1]], "warning_rlang")

  expect_equal(
    signal_info("message", base::message, ""),
    list("message", quote(f()))
  )
  expect_equal(
    signal_info("message", base::message, cnd("message")),
    list("message", quote(f()))
  )
  expect_equal(signal_info("message", inform, "")[[1]], "message_rlang")

  expect_equal(
    signal_info("condition", base::signalCondition, cnd("foo")),
    list("condition", quote(f()))
  )

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

test_that("cnd_entrace() skips capture context", {
  capture <- function(expr) {
    env <- environment()
    withCallingHandlers(
      expr,
      error = function(err) {
        err <- cnd_entrace(err)
        return_from(env, err)
      }
    )
  }
  foo <- function() bar()
  bar <- function() stop("foobar")

  local_options(rlang_trace_top_env = current_env())
  err <- capture(foo())

  last <- err$trace$call[[4]]
  expect_match(deparse(last), "bar")
})

test_that("rlang and base errors are properly entraced", {
  skip_if_stale_backtrace()

  base <- run_script(test_path("fixtures", "error-entrace.R"))

  rlang <- run_script(
    test_path("fixtures", "error-entrace.R"),
    envvars = "rlang_error_kind=rlang"
  )

  expect_snapshot({
    cat_line(base)
    cat_line(rlang)
  })
})

test_that("entrace() preserves exit status in non-interactive sessions (#1052, rstudio/bookdown#920)", {
  # Probably because of <https://github.com/wch/r-source/commit/3055aa86>
  skip_if(getRversion() < "3.3")

  # This also tests for empty backtraces
  out <- Rscript(shQuote(c(
    "--vanilla",
    "-e",
    'options(error = rlang::entrace); stop("An error")'
  )))
  expect_false(out$status == 0L)

  code <- '{
    options(error = rlang::entrace)
    f <- function() g()
    g <- function() h()
    h <- function() stop("An error")
    f()
  }'
  out <- Rscript(shQuote(c("--vanilla", "-e", code)))
  expect_false(out$status == 0L)
})

test_that("entrace() doesn't embed backtraces twice", {
  skip_if_stale_backtrace()

  code <- "withCallingHandlers(error = rlang::entrace, rlang::abort('foo'))"
  out <- Rscript(shQuote(c("--vanilla", "-e", code)))$out

  expect_equal(sum(grepl("^Backtrace", out)), 1)
})

test_that("`options(error = entrace)` strips error prefix", {
  code <- '
  {
    options(error = rlang::entrace)
    f <- function() g()
    g <- function() h()
    h <- function() 1 + ""
    f()
    last_error()
  }'
  out <- Rscript(shQuote(c("--vanilla", "-e", code)))

  expect_false(out$status == 0L)
})

test_that("can supply handler environment as `bottom`", {
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )

  f <- function() g()
  g <- function() h()
  h <- function() identity(1 + "")

  err <- catch_cnd(
    withCallingHandlers(
      error = function(...) rlang::entrace(..., bottom = environment()),
      f()
    ),
    "error"
  )
  expect_snapshot(print(err))
})

test_that("can set `entrace()` as a global handler", {
  expect_snapshot_output(run(
    '{
    suppressMessages(testthat::local_reproducible_output())
    rlang::global_entrace()
    f <- function() g()
    g <- function() h()
    h <- function() 1 + ""
    f()
  }'
  ))

  # Indirected case for developers of rlang
  expect_snapshot_output(run(
    '{
    suppressMessages(testthat::local_reproducible_output())
    globalCallingHandlers(error = function(...) rlang::entrace(..., bottom = environment()))
    f <- function() g()
    g <- function() h()
    h <- function() 1 + ""
    f()
  }'
  ))

  expect_snapshot_output(run(
    '{
    suppressMessages(testthat::local_reproducible_output())
    rlang::global_entrace()
    f <- function() { warning("foo"); message("FOO"); g() }
    g <- function() { warning("bar", immediate. = TRUE); h() }
    h <- function() message("baz")
    f()
    writeLines("> rlang::last_warnings()")
    print(rlang::last_warnings())

    writeLines("\\n> rlang::last_warnings(2)")
    print(rlang::last_warnings(2))

    writeLines("\\n> summary(rlang::last_messages())")
    summary(rlang::last_messages())

    writeLines("\\n> summary(rlang::last_messages(1))")
    summary(rlang::last_messages(1))
  }'
  ))
})

test_that("errors are saved by `entrace()`", {
  out <- tryCatch(
    withCallingHandlers(
      abort("foo"),
      error = entrace
    ),
    error = identity
  )

  # Remove internal data stored by `last_error()`
  err <- last_error()
  err$rlang <- NULL
  out$rlang <- NULL

  expect_equal(err, out)
})

test_that("only the first n warnings are entraced (#1473)", {
  suppressWarnings({
    local_options(
      "rlang:::cnd_frame" = current_env(),
      "rlang:::max_entracing" = 3L
    )

    f <- function() g()
    g <- function() h()
    h <- function() warning("foo")

    try_fetch(
      warning = function(cnd) {
        entrace(cnd)
        zap()
      },
      for (i in 1:5) {
        f()
      }
    )

    expect_equal(
      map_lgl(last_warnings(), function(x) is_null(x$trace)),
      c(FALSE, FALSE, FALSE, TRUE, TRUE)
    )
  })
})

test_that("warnings are resignalled", {
  expect_no_warning(
    cnd <- catch_cnd(withCallingHandlers(
      warning = entrace,
      warning("foo")
    ))
  )

  expect_s3_class(cnd, "rlang_warning")
  expect_true(!is_null(cnd$trace))
})

test_that("can call `global_entrace()` in knitted documents", {
  local_options(
    rlang_backtrace_on_error_report = peek_option(
      "rlang_backtrace_on_error_report"
    ),
    rlang_backtrace_on_warning_report = peek_option(
      "rlang_backtrace_on_warning_report"
    )
  )
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  entrace_lines <- render_md("test-entrace.Rmd", env = current_env())

  expect_snapshot({
    cat_line(entrace_lines)
  })
})

test_that("can't set backtrace-on-warning to reminder", {
  local_options(rlang_backtrace_on_warning_report = "reminder")

  expect_snapshot({
    peek_backtrace_on_warning_report()
  })

  expect_equal(
    peek_option("rlang_backtrace_on_warning_report"),
    "none"
  )
})

test_that("warnings converted to errors are not resignalled by `global_entrace()`", {
  local_options(warn = 2)

  out <- withCallingHandlers(
    warning = entrace,
    tryCatch(error = function(...) "ok", warning("foo"))
  )

  expect_equal(out, "ok")
})
