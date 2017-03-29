context("conditions") # ----------------------------------------------

test_that("new_cnd() constructs all fields", {
  cond <- new_cnd("cnd_class", .msg = "cnd message")
  expect_equal(conditionMessage(cond), "cnd message")
  expect_is(cond, "cnd_class")
})

test_that("new_cnd() throws with unnamed fields", {
  expect_error(new_cnd("class", "msg", 10), "must have named data fields")
})

test_that("cnd_signal() creates muffle restarts", {
  withCallingHandlers(cnd_signal("foo", muffle = TRUE),
    foo = function(c) {
      expect_true(rst_exists("muffle"))
      expect_is(c, "mufflable")
    }
  )
})

test_that("cnd_signal() include call info", {
  cnd <- new_cnd("cnd", .call = quote(foo(bar)))
  fn <- function(...) cnd_signal(cnd, .call = call)

  call <- FALSE
  with_handlers(fn(foo(bar)), cnd = inplace(function(c) {
    expect_identical(c$.call, quote(fn(foo(bar))))
    expect_null(conditionCall(c))
  }))

  call <- TRUE
  with_handlers(fn(foo(bar)), cnd = inplace(function(c) {
    expect_identical(conditionCall(c), quote(fn(foo(bar))))
  }))
})

test_that("abort() includes call info", {
  fn <- function(...) abort("abort", "cnd", call = call)

  call <- FALSE
  with_handlers(fn(foo(bar)), cnd = exiting(function(c) {
    expect_identical(c$.call, quote(fn(foo(bar))))
    expect_null(conditionCall(c))
  }))

  call <- TRUE
  with_handlers(fn(foo(bar)), cnd = exiting(function(c) {
    expect_identical(conditionCall(c), quote(fn(foo(bar))))
  }))
})

test_that("error when msg is not a string", {
  expect_error(warn(letters), "must be a string")
})


context("restarts") # ------------------------------------------------

test_that("restarts are established", {
  with_restarts(foo = function() {}, expect_true(rst_exists("foo")))
})


context("handlers") # ------------------------------------------------

test_that("Local handlers can muffle mufflable conditions", {
  signal_mufflable <- function() cnd_signal("foo", with_muffle = TRUE)
  muffling_handler <- inplace(function(c) NULL, muffle = TRUE)
  non_muffling_handler <- inplace(function(c) NULL, muffle = FALSE)

  expect_error(regexp = "not muffled!",
    withCallingHandlers(foo = function(c) stop("not muffled!"), {
      withCallingHandlers(foo = non_muffling_handler,
        signal_mufflable())
    }))

  expect_error(regexp = NA,
    withCallingHandlers(foo = function(c) stop("not muffled!"), {
      withCallingHandlers(foo = muffling_handler,
        signal_mufflable())
    }))
})

test_that("with_handlers() establishes inplace and exiting handlers", {
  handlers <- list(
    error = exiting(function(c) "caught error"),
    message = exiting(function(c) "caught message"),
    warning = inplace(function(c) "warning"),
    foobar = inplace(function(c) cat("foobar"))
  )

  expect_equal(with_handlers(identity(letters), splice(handlers)), identity(letters))
  expect_equal(with_handlers(stop(letters), splice(handlers)), "caught error")
  expect_equal(with_handlers(message(letters), splice(handlers)), "caught message")
  expect_warning(expect_equal(with_handlers({ warning("warn!"); letters }, splice(handlers)), identity(letters)), "warn!")
  expect_output(expect_equal(with_handlers({ cnd_signal("foobar"); letters }, splice(handlers)), identity(letters)), "foobar")
})

test_that("set_names2() fills in empty names", {
  chr <- c("a", b = "B", "c")
  expect_equal(set_names2(chr), c(a = "a", b = "B", c = "c"))
})

test_that("restarting() handlers pass along all requested arguments", {
  signal_foo <- function() {
    cnd_signal("foo", foo_field = "foo_field")
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
