context("language")

test_that("NULL is a valid language object", {
  expect_true(is_lang(NULL))
})

# coercion ----------------------------------------------------------------

test_that("as_name() produces names", {
  expect_equal(as_name("a"), quote(a))
  expect_equal(as_name(quote(a)), quote(a))
  expect_equal(as_name(quote(a())), quote(a))
  expect_equal(as_name(~ a), quote(a))
  expect_equal(as_name(~ a()), quote(a))

  expect_error(as_name(c("a", "b")), "Cannot read character vector of length > 1")
})

test_that("as_call() produces calls", {
  expect_equal(as_call(quote(a)), quote(a()))
  expect_equal(as_call(quote(a())), quote(a()))
  expect_equal(as_call("a()"), quote(a()))
  expect_equal(as_call(~ a()), quote(a()))

  expect_error(as_call(c("a", "b")), "Cannot read character vector of length > 1")
})

test_that("as_name() handles prefixed call names", {
  expect_identical(as_name(quote(foo::bar())), quote(foo::bar))
  expect_identical(as_name(~foo@bar()), quote(foo@bar))
})
