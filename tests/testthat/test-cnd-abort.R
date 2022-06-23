local_unexport_signal_abort()

test_that("errors are signalled with backtrace", {
  fn <- function() abort("")
  err <- expect_error(fn())
  expect_s3_class(err$trace, "rlang_trace")
})

test_that("can pass classed strings as error message", {
  message <- structure("foo", class = c("glue", "character"))
  err <- expect_error(abort(message))
  expect_identical(err$message, message)
})

test_that("errors are saved", {
  # `outFile` argument
  skip_if(getRversion() < "3.4")

  file <- tempfile()
  on.exit(unlink(file))

  local_options(
    `rlang::::force_unhandled_error` = TRUE,
    `rlang:::message_file` = tempfile()
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
  local_options(
    `rlang::::force_unhandled_error` = TRUE,
    `rlang:::message_file` = tempfile(),
    rlang_backtrace_on_error = NA
  )
  expect_snapshot({
    (expect_warning(tryCatch(abort("foo"), error = identity)))
  })
  expect_null(peek_option("rlang_backtrace_on_error"))
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
  err <- catch_error(f())

  poke_last_error(err)

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
      poke_last_error(error_cnd("foo"))
      print(saved)
    }
  })
})

local({
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )

  throw <- NULL

  low1 <- function() low2()
  low2 <- function() low3()
  low3 <- NULL

  high3_catch <- function(..., chain, stop_helper) {
    tryCatch(
      low1(),
      error = function(err) {
        parent <- if (chain) err else NA
        if (stop_helper) {
          stop1("high-level", parent = err)
        } else {
          abort("high-level", parent = err)
        }
      }
    )
  }
  high3_call <- function(..., chain, stop_helper) {
    withCallingHandlers(
      low1(),
      error = function(err) {
        parent <- if (chain) err else NA
        if (stop_helper) {
          stop1("high-level", parent = err)
        } else {
          abort("high-level", parent = err)
        }
      }
    )
  }
  high3_fetch <- function(..., chain, stop_helper) {
    try_fetch(
      low1(),
      error = function(err) {
        parent <- if (chain) err else NA
        if (stop_helper) {
          stop1("high-level", parent = err)
        } else {
          abort("high-level", parent = err)
        }
      }
    )
  }

  high1 <- function(...) high2(...)
  high2 <- function(...) high3(...)
  high3 <- NULL

  stop1 <- function(..., call = caller_env()) stop2(..., call = call)
  stop2 <- function(..., call = caller_env()) stop3(..., call = call)
  stop3 <- function(..., call = caller_env()) abort(..., call = call)

  throwers <- list(
    "stop()" = function() stop("low-level"),
    "abort()" = function() abort("low-level"),
    "warn = 2" = function() {
      local_options(warn = 2)
      warning("low-level")
    }
  )
  handlers <- list(
    "tryCatch()" = high3_catch,
    "withCallingHandlers()" = high3_call,
    "try_fetch()" = high3_fetch
  )

  for (i in seq_along(throwers)) {
    for (j in seq_along(handlers)) {
      case_name <- paste0(
        "Backtrace on rethrow: ",
        names(throwers)[[i]], " - ", names(handlers)[[j]]
      )
      low3 <- throwers[[i]]
      high3 <- handlers[[j]]

      # Use `print()` because `testthat_print()` (called implicitly in
      # snapshots) doesn't print backtraces
      test_that(case_name, {
        expect_snapshot({
          print(catch_error(high1(chain = TRUE, stop_helper = TRUE)))
          print(catch_error(high1(chain = TRUE, stop_helper = FALSE)))

          print(catch_error(high1(chain = FALSE, stop_helper = TRUE)))
          print(catch_error(high1(chain = FALSE, stop_helper = FALSE)))
        })
      })
    }
  }
})

test_that("abort() displays call in error prefix", {
  skip_if_not_installed("rlang", "0.4.11.9001")

  expect_snapshot(
    run("{
      options(cli.unicode = FALSE, crayon.enabled = FALSE)
      rlang::abort('foo', call = quote(bar(baz)))
    }")
  )

  # errorCondition()
  skip_if_not_installed("base", "3.6.0")

  expect_snapshot(
    run("{
      options(cli.unicode = FALSE, crayon.enabled = FALSE)
      rlang::cnd_signal(errorCondition('foo', call = quote(bar(baz))))
    }")
  )
})

test_that("abort() accepts environment as `call` field.", {
  check_required2 <- function(arg, call = caller_call()) {
    check_required(arg, call = call)
  }
  f <- function(x) g(x)
  g <- function(x) h(x)
  h <- function(x) check_required2(x, call = environment())

  expect_snapshot((expect_error(f())))
})

test_that("format_error_arg() formats argument", {
  exp <- format_arg("foo")

  expect_equal(format_error_arg("foo"), exp)
  expect_equal(format_error_arg(sym("foo")), exp)
  expect_equal(format_error_arg(chr_get("foo", 0L)), exp)
  expect_equal(format_error_arg(quote(foo())), format_arg("foo()"))

  expect_error(format_error_arg(c("foo", "bar")), "must be a string or an expression")
  expect_error(format_error_arg(function() NULL), "must be a string or an expression")
})

test_that("local_error_call() works", {
  foo <- function() {
    bar()
  }
  bar <- function() {
    local_error_call(quote(expected()))
    baz()
  }
  baz <- function() {
    local_error_call("caller")
    abort("tilt")
  }

  expect_snapshot((expect_error(foo())))
})

test_that("can disable error call inference for unexported functions", {
  foo <- function() abort("foo")

  expect_snapshot({
    (expect_error(foo()))

    local({
      local_options("rlang:::restrict_default_error_call" = TRUE)
      (expect_error(foo()))
    })

    local({
      local_options("rlang:::restrict_default_error_call" = TRUE)
      (expect_error(dots_list(.homonyms = "k")))
    })
  })
})

test_that("error call flag is stripped", {
  e <- env(.__error_call__. = quote(foo(bar)))
  expect_equal(error_call(e), quote(foo(bar)))
  expect_equal(format_error_call(e), "`foo()`")
})

test_that("NSE doesn't interfere with error call contexts", {
  # Snapshots shouldn't show `eval()` as context
  expect_snapshot({
    (expect_error(local(arg_match0("f", "foo"))))
    (expect_error(eval_bare(quote(arg_match0("f", "foo")))))
    (expect_error(eval_bare(quote(arg_match0("f", "foo")), env())))
  })
})

test_that("error_call() requires a symbol in function position", {
  expect_null(format_error_call(quote((function() NULL)())))
  expect_null(format_error_call(call2(function() NULL)))
})

test_that("error_call() preserves index calls", {
  expect_equal(format_error_call(quote(foo$bar(...))), "`foo$bar()`")
})

test_that("error_call() preserves `if` (r-lib/testthat#1429)", {
  call <- quote(if (foobar) TRUE else FALSE)

  expect_equal(
    error_call(call),
    call
  )
  expect_equal(
    format_error_call(call),
    "`if (foobar) ...`"
  )
})

test_that("error_call() and format_error_call() preserve special syntax ops", {
  expect_equal(
    error_call(quote(1 + 2)),
    quote(1 + 2)
  )
  expect_snapshot(format_error_call(quote(1 + 2)))

  expect_equal(
    error_call(quote(for (x in y) NULL)),
    quote(for (x in y) NULL)
  )
  expect_snapshot(format_error_call(quote(for (x in y) NULL)))

  expect_snapshot(format_error_call(quote(a %||% b)))
  expect_snapshot(format_error_call(quote(`%||%`())))  # Suboptimal
})

test_that("error_call() preserves srcrefs", {
  eval_parse("{
    f <- function() g()
    g <- function() h()
    h <- function() abort('Foo.')
  }")

  out <- error_call(catch_error(f())$call)
  expect_s3_class(attr(out, "srcref"), "srcref")
})

test_that("withCallingHandlers() wrappers don't throw off trace capture on rethrow", {
  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  variant <- if (getRversion() < "3.6.0") "pre-3.6.0" else "current"

  low1 <- function() low2()
  low2 <- function() low3()
  low3 <- function() abort("Low-level message")

  wch <- function(expr, ...) {
    withCallingHandlers(expr, ...)
  }
  handler1 <- function(err, call = caller_env()) {
    handler2(err, call = call)
  }
  handler2 <- function(err, call = caller_env()) {
    abort("High-level message", parent = err, call = call)
  }

  high1 <- function() high2()
  high2 <- function() high3()
  high3 <- function() {
    wch(
      low1(),
      error = function(err) handler1(err)
    )
  }

  err <- expect_error(high1())
  expect_snapshot(variant = variant, {
    "`abort()` error"
    print(err)
    summary(err)
  })

  # Avoid `:::` vs `::` ambiguity depending on loadall
  fail <- errorcall
  low3 <- function() fail(NULL, "Low-level message")
  err <- expect_error(high1())
  expect_snapshot(variant = variant, {
    "C-level error"
    print(err)
    summary(err)
  })
})

test_that("headers and body are stored in respective fields", {
  local_use_cli()  # Just to be explicit

  cnd <- catch_cnd(abort(c("a", "b", i = "c")), "error")
  expect_equal(cnd$message, set_names("a", ""))
  expect_equal(cnd$body, c("b", i = "c"))
})

test_that("`abort()` uses older bullets formatting by default", {
  local_use_cli(format = FALSE)
  expect_snapshot_error(abort(c("foo", "bar")))
})

test_that("abort() preserves `call`", {
  err <- catch_cnd(abort("foo", call = quote(1 + 2)), "error")
  expect_equal(err$call, quote(1 + 2))
})

test_that("format_error_call() preserves I() inputs", {
  expect_equal(
    format_error_call(I(quote(.data[[1]]))),
    "`.data[[1]]`"
  )
})

test_that("format_error_call() detects non-syntactic names", {
  expect_equal(
    format_error_call(quote(`[[.foo`())),
    "`[[.foo`"
  )
})

test_that("generic call is picked up in methods", {
  g <- function(call = caller_env()) {
    abort("foo", call = call)
  }

  f1 <- function(x) {
    UseMethod("f1")
  }
  f1.default <- function(x) {
    g()
  }

  f2 <- function(x) {
    UseMethod("f2")
  }
  f2.NULL <- function(x) {
    NextMethod()
  }
  f2.default <- function(x) {
    g()
  }

  f3 <- function(x) {
    UseMethod("f3")
  }
  f3.foo <- function(x) {
    NextMethod()
  }
  f3.bar <- function(x) {
    NextMethod()
  }
  f3.default <- function(x) {
    g()
  }

  f4 <- function(x) {
    f4_dispatch(x)
  }
  f4_dispatch <- function(x) {
    local_error_call("caller")
    UseMethod("f4")
  }
  f4.foo <- function(x) {
    NextMethod()
  }
  f4.bar <- function(x) {
    NextMethod()
  }
  f4.default <- function(x) {
    g()
  }

  expect_snapshot({
    err(f1())
    err(f2())
    err(f3())
    err(f4(NULL))
  })
})

test_that("errors are fully displayed (parents, calls) in knitted files", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available())

  expect_snapshot({
    writeLines(render_md("test-parent-errors.Rmd"))
  })
})

test_that("can supply bullets both through `message` and `body`", {
  local_use_cli(format = FALSE)
  expect_snapshot({
    (expect_error(abort("foo", body = c("a", "b"))))
    (expect_error(abort(c("foo", "bar"), body = c("a", "b"))))
  })
})

test_that("can supply bullets both through `message` and `body` (cli case)", {
  local_use_cli(format = TRUE)
  expect_snapshot({
    (expect_error(abort("foo", body = c("a", "b"))))
    (expect_error(abort(c("foo", "bar"), body = c("a", "b"))))
  })
})

test_that("setting `.internal` adds footer bullet", {
  expect_snapshot({
    err(abort(c("foo", "x" = "bar"), .internal = TRUE))
    err(abort("foo", body = c("x" = "bar"), .internal = TRUE))
  })
})

test_that("setting `.internal` adds footer bullet (fallback)", {
  local_use_cli(format = FALSE)
  expect_snapshot({
    err(abort(c("foo", "x" = "bar"), .internal = TRUE))
    err(abort("foo", body = c("x" = "bar"), .internal = TRUE))
  })
})

test_that("must pass character `body` when `message` is > 1", {
  expect_snapshot({
    # This is ok because `message` is length 1
    err(abort("foo", body = function(cnd, ...) c("i" = "bar")))

    # This is an internal error
    err(abort(c("foo", "bar"), body = function() "baz"))
  })
})

test_that("must pass character `body` when `message` is > 1 (non-cli case)", {
  local_use_cli(format = FALSE)
  expect_snapshot({
    # This is ok because `message` is length 1
    err(abort("foo", body = function(cnd, ...) c("i" = "bar")))

    # This is an internal error
    err(abort(c("foo", "bar"), body = function() "baz"))
  })
})

test_that("can supply `footer`", {
  local_error_call(call("f"))
  expect_snapshot({
    err(abort("foo", body = c("i" = "bar"), footer = c("i" = "baz")))
    err(abort("foo", body = function(cnd, ...) c("i" = "bar"), footer = function(cnd, ...) c("i" = "baz")))
  })
})

test_that("can supply `footer` (non-cli case)", {
  local_use_cli(format = FALSE)
  local_error_call(call("f"))
  expect_snapshot({
    err(abort("foo", body = c("i" = "bar"), footer = c("i" = "baz")))
    err(abort("foo", body = function(cnd, ...) c("i" = "bar"), footer = function(cnd, ...) c("i" = "baz")))
  })
})

test_that("can't supply both `footer` and `.internal`", {
  expect_snapshot({
    err(abort("foo", .internal = TRUE, call = quote(f())))
    err(abort("foo", footer = "bar", .internal = TRUE, call = quote(f())))
  })
})

test_that("caller of withCallingHandlers() is used as default `call`", {
  low <- function() {
    # Intervening `withCallingHandlers()` is not picked up
    withCallingHandlers(stop("low"))
  }
  high <- function() {
    withCallingHandlers(
      low(),
      error = function(cnd) abort("high", parent = cnd)
    )
  }
  err <- catch_error(high())
  expect_equal(err$call, quote(high()))

  # Named case
  handler <- function(cnd) abort("high", parent = cnd)
  high <- function() {
    withCallingHandlers(
      low(),
      error = handler
    )
  }
  err <- catch_error(high())
  expect_equal(err$call, quote(high()))

  # Wrapped case
  handler1 <- function(cnd) handler2(cnd)
  handler2 <- function(cnd) abort("high", parent = cnd)
  high <- function() {
    try_fetch(
      low(),
      error = handler1
    )
  }
  err <- catch_error(high())
  expect_equal(err$call, quote(high()))

  function(cnd) abort("high", parent = cnd)
})

test_that("`cli.condition_unicode_bullets` is supported by fallback formatting", {
  local_use_cli(format = FALSE)
  local_options(
    cli.unicode = TRUE,
    cli.condition_unicode_bullets = FALSE
  )
  expect_snapshot_error(
    rlang::abort(c("foo", "i" = "bar"))
  )
})

test_that("call can be a quosure or contain quosures", {
  err <- catch_error(abort("foo", call = quo(f(!!quo(g())))))
  expect_equal(err$call, quote(f(g())))
})

test_that("`parent = NA` signals a non-chained rethrow", {
  variant <- if (getRversion() < "3.6.0") "pre-3.6.0" else "current"

  local_options(
    rlang_trace_top_env = current_env(),
    rlang_trace_format_srcrefs = FALSE
  )

  ff <- function() gg()
  gg <- function() hh()

  foo <- function() bar()
  bar <- function() baz()
  baz <- function() stop("bar")

  expect_snapshot(variant = variant, {
    "Absent parent causes bad trace bottom"
    hh <- function() {
      withCallingHandlers(foo(), error = function(cnd) {
        abort(cnd_header(cnd))
      })
    }
    print(err(ff()))

    "Missing parent allows correct trace bottom"
    hh <- function() {
      withCallingHandlers(foo(), error = function(cnd) {
        abort(cnd_header(cnd), parent = NA)
      })
    }
    print(err(ff()))

    "Wrapped handler"
    handler1 <- function(cnd, call = caller_env()) handler2(cnd, call)
    handler2 <- function(cnd, call) abort(cnd_header(cnd), parent = NA, call = call)
    hh <- function() {
      withCallingHandlers(
        foo(),
        error = function(cnd) handler1(cnd)
      )
    }
    print(err(ff()))

    "Wrapped handler, `try_fetch()`"
    hh <- function() {
      try_fetch(
        foo(),
        error = function(cnd) handler1(cnd)
      )
    }
    print(err(ff()))

    "Wrapped handler, incorrect `call`"
    hh <- function() {
      withCallingHandlers(
        foo(),
        error = handler1
      )
    }
    print(err(ff()))
  })
})

test_that("can rethrow outside handler", {
  local_options(rlang_trace_format_srcrefs = FALSE)

  parent <- error_cnd(message = "Low-level", call = call("low"))

  foo <- function() bar()
  bar <- function() baz()
  baz <- function() abort("High-level", parent = parent)

  expect_snapshot({
    print(err(foo()))
  })
})

test_that("if `call` is older than handler caller, use that as bottom", {
  local_options(
    rlang_trace_format_srcrefs = FALSE
  )
  f <- function() helper()
  helper <- function(call = caller_env()) {
    try_fetch(
      low_level(call),
      error = function(cnd) abort(
        "Problem.",
        parent = cnd,
        call = call
      )
    )
  }

  expect_snapshot({
    low_level <- function(call) {
      abort("Tilt.", call = call)
    }
    print(expect_error(f()))

    low_level <- function(call) {
      abort("Tilt.", call = list(NULL, frame = call))
    }
    print(expect_error(f()))
  })
})

test_that("is_calling_handler_inlined_call() doesn't fail with OOB subsetting", {
  expect_false(is_calling_handler_inlined_call(call2(function() NULL)))
})

test_that("base causal errors include full user backtrace", {
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )

  my_verb <- function(expr) {
    with_chained_errors(expr)
  }
  with_chained_errors <- function(expr, call = caller_env()) {
    try_fetch(
      expr,
      error = function(cnd) {
        abort(
          "Problem during step.",
          parent = cnd,
          call = call
        )
      }
    )
  }

  add <- function(x, y) x + y
  expect_snapshot({
    print(expect_error(my_verb(add(1, ""))))
  })
})

test_that("can chain errors at top-level (#1405)", {
  out <- run_code("
    tryCatch(
      error = function(err) rlang::abort('bar', parent = err),
      rlang::abort('foo')
    )
  ")
  expect_true(any(grepl("foo", out$output)))
  expect_true(any(grepl("bar", out$output)))

  out <- run_code("
    withCallingHandlers(
      error = function(err) rlang::abort('bar', parent = err),
      rlang::abort('foo')
    )
  ")
  expect_true(any(grepl("foo", out$output)))
  expect_true(any(grepl("bar", out$output)))
})

test_that("backtrace_on_error = 'collapse' is deprecated.", {
  local_options("rlang_backtrace_on_error" = "collapse")
  expect_warning(peek_backtrace_on_error(), "deprecated")
  expect_equal(peek_option("rlang_backtrace_on_error"), "none")
})

test_that("can supply header method via `message`", {
  expect_snapshot(error = TRUE, {
    abort(~ "foo")
    abort(function(cnd, ...) "foo")
  })

  msg <- function(cnd, ...) "foo"
  cnd <- catch_error(abort(msg))
  expect_identical(cnd$header, msg)

  expect_error(
    abort(function(cnd) "foo"),
    "must take"
  )
})
