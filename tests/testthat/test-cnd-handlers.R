context("cnd-handlers")

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
