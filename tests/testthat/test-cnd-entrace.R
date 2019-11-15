context("cnd-entrace")

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

  local_options(
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

  local_options(
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

  # No longer works since we switched to signalCondition approach
  # expect_equal(signal_info(abort, "")[[1]], "stop_rlang")

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

  last <- err$trace$calls[[4]]
  expect_match(deparse(last), "bar")
})
