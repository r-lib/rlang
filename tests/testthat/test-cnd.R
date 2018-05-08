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

test_that("signal() includes call info", {
  fn <- function(...) signal("msg", "cnd", call = call)

  call <- NULL
  with_handlers(fn(foo(bar)), cnd = calling(function(c) {
    expect_null(conditionCall(c))
  }))

  call <- TRUE
  with_handlers(fn(foo(bar)), cnd = calling(function(c) {
    expect_identical(conditionCall(c), quote(fn(foo(bar))))
  }))


  wrapper <- function(...) fn(...)

  call <- 1
  with_handlers(wrapper(foo(bar)), cnd = calling(function(c) {
    expect_equal(conditionCall(c), quote(fn(...)))
  }))

  call <- 2
  with_handlers(wrapper(foo(bar)), cnd = calling(function(c) {
    expect_equal(conditionCall(c), quote(wrapper(foo(bar))))
  }))
})

test_that("abort() includes call info", {
  fn <- function(...) abort("abort", "cnd", call = call)

  call <- NULL
  with_handlers(fn(foo(bar)), cnd = exiting(function(c) {
    expect_null(conditionCall(c))
  }))

  call <- TRUE
  with_handlers(fn(foo(bar)), cnd = exiting(function(c) {
    expect_identical(conditionCall(c), quote(fn(foo(bar))))
  }))
})

test_that("abort() accepts call number", {
  fn <- function(...) abort("abort", "cnd", call = call)
  wrapper <- function(...) fn(...)

  call <- FALSE
  with_handlers(wrapper(foo(bar)), cnd = exiting(function(c) {
    expect_null(conditionCall(c))
  }))

  call <- TRUE
  with_handlers(wrapper(foo(bar)), cnd = exiting(function(c) {
    expect_equal(conditionCall(c), quote(fn(...)))
  }))

  call <- 1
  with_handlers(wrapper(foo(bar)), cnd = exiting(function(c) {
    expect_equal(conditionCall(c), quote(fn(...)))
  }))

  call <- 2
  with_handlers(wrapper(foo(bar)), cnd = exiting(function(c) {
    expect_equal(conditionCall(c), quote(wrapper(foo(bar))))
  }))

  expect_error(abort("foo", call = na_int), "scalar logical or number")
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


# Lifecycle ----------------------------------------------------------

test_that("deprecated arguments of abort() etc still work", {
  foo <- function() {
    abort(msg = "foo", type = "bar", call = TRUE)
  }

  cnds <- catch_cnds(foo())
  msgs <- pluck_conditions_msgs(cnds)

  warnings_msgs <- msgs$warnings
  expect_length(warnings_msgs, 2L)
  expect_match(warnings_msgs[[1]], "`msg` has been renamed to `message`")
  expect_match(warnings_msgs[[2]], "`type` has been renamed to `.subclass`")

  expect_match(msgs$error, "foo")
  expect_identical(conditionCall(cnds$error), quote(foo()))
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
