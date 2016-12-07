context("conditions") # ----------------------------------------------

test_that("cnd_new() constructs all fields", {
  cond <- cnd_new("cnd_class", .msg = "cnd message")
  expect_equal(conditionMessage(cond), "cnd message")
  expect_is(cond, "cnd_class")
})

test_that("cnd_new() throws with unnamed fields", {
  expect_error(cnd_new("class", "msg", 10), "must have named data fields")
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
  cnd <- cnd_new("cnd", .call = quote(foo(bar)))
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
  with_handlers(fn(foo(bar)), cnd = thrown(function(c) {
    expect_identical(c$.call, quote(fn(foo(bar))))
    expect_null(conditionCall(c))
  }))

  call <- TRUE
  with_handlers(fn(foo(bar)), cnd = thrown(function(c) {
    expect_identical(conditionCall(c), quote(fn(foo(bar))))
  }))
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

test_that("with_handlers() constructs correct expression", {
  inplace <- list(foo = function(c) "foo", bar = function(c) "bar")
  thrown <- list(baz = function(c) "baz", bam = function(c) "bam")
  f <- interp_handlers(~expr(), inplace = inplace, thrown = thrown)

  expected_expr <- bquote(withCallingHandlers(
    tryCatch(expr(),
      baz = .(thrown$baz),
      bam = .(thrown$bam)
    ),
    foo = .(inplace$foo),
    bar = .(inplace$bar)
  ))

  expect_identical(f_rhs(f), expected_expr)
})

test_that("with_handlers() establishes inplace and thrown handlers", {
  handlers <- list(
    error = thrown(function(c) "caught error"),
    message = thrown(function(c) "caught message"),
    warning = inplace(function(c) "warning"),
    foobar = inplace(function(c) cat("foobar"))
  )

  expect_equal(with_handlers(identity(letters), .handlers = handlers), identity(letters))
  expect_equal(with_handlers(stop(letters), .handlers = handlers), "caught error")
  expect_equal(with_handlers(message(letters), .handlers = handlers), "caught message")
  expect_warning(expect_equal(with_handlers({ warning("warn!"); letters }, .handlers = handlers), identity(letters)), "warn!")
  expect_output(expect_equal(with_handlers({ cnd_signal("foobar"); letters }, .handlers = handlers), identity(letters)), "foobar")
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
    .args = list(b = "b"),
    .fields = c(field_arg = "foo_field")
  )

  rst_foo <- function(a, b, field_arg) {
    expect_equal(list(a, b, field_arg), list("a", "b", "foo_field"))
  }
  with_restarts(fn(), rst_foo = rst_foo)
})
