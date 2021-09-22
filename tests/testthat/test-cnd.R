test_that("cnd() constructs all fields", {
  cond <- cnd("cnd_class", message = "cnd message")
  expect_identical(conditionMessage(cond), "cnd message")
  expect_s3_class(cond, "cnd_class")
})

test_that("cnd() throws with unnamed fields", {
  expect_error(cnd("class", "msg", 10), "must have named data fields")
})

test_that("restarts are established", {
  with_restarts(foo = function() {}, expect_true(rst_exists("foo")))
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

test_that("bare conditions must be subclassed", {
  expect_error(cnd(), "must be subclassed")
  expect_error(signal(""), "must be subclassed")
})

test_that("predicates match condition classes", {
  expect_true(is_error(catch_cnd(stop("foo"))))
  expect_false(is_error(catch_cnd(warning("foo"))))
  expect_true(is_warning(catch_cnd(warning("foo"))))
  expect_true(is_message(catch_cnd(message("foo"))))
})

test_that("warn() and inform() signal subclassed conditions", {
  wrn <- catch_cnd(warn(""), "warning")
  msg <- catch_cnd(inform(""), "message")
  expect_equal(class(wrn), c("rlang_warning", "warning", "condition"))
  expect_equal(class(msg), c("rlang_message", "message", "condition"))
})

test_that("check for duplicate condition fields (#1268)", {
  expect_error(error_cnd("foo", foo = 1, foo = 2), "same name")
  expect_error(abort("", foo = 1, foo = 2), "same name")
})

test_that("cnd_type_header() formats condition classes", {
  expect_snapshot({
    cnd_type_header(error_cnd())
    cnd_type_header(warning_cnd())
    cnd_type_header(message_cnd())
    cnd_type_header(error_cnd(class = "foobar"))
  })
})

test_that("can format warnings and other conditions", {
  trace <- new_trace(alist(foo(), bar()), 0:1)

  warning <- warning_cnd(
    message = c("Header.", i = "Bullet."),
    call = quote(quux()),
    use_cli_format = TRUE,
    trace = trace
  )
  expect_snapshot_output(cnd_print(warning))

  message <- message_cnd(
    message = c("Header.", i = "Bullet."),
    call = quote(quux()),
    use_cli_format = TRUE,
    trace = trace,
    parent = warning
  )
  expect_snapshot_output(cnd_print(message))

  condition <- cnd(
    "foobar",
    message = c("Header.", i = "Bullet."),
    call = quote(quux()),
    use_cli_format = TRUE,
    trace = trace
  )
  expect_snapshot_output(cnd_print(condition))
})

test_that("warnings and messages have `summary()` methods", {
  warning <- warning_cnd(trace = new_trace(alist(f(), g()), 0:1))
  message <- message_cnd(trace = new_trace(alist(f(), g()), 0:1))
  expect_snapshot({
    print(warning)
    print(message)
    summary(warning)
    summary(message)
  })
})

test_that("cnd ctors check arguments", {
  expect_snapshot({
    (expect_error(warning_cnd(class = list())))
    (expect_error(error_cnd(class = list())))
    (expect_error(message_cnd(message = 1)))
  })
})
