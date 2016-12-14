context("language")

test_that("NULL is a valid language object", {
  expect_true(is_lang(NULL))
})

test_that("is_call() pattern-matches", {
  expect_true(is_call(quote(foo(bar)), "foo"))
  expect_false(is_call(quote(foo(bar)), "bar"))
  expect_true(is_call(quote(foo(bar)), quote(foo)))

  expect_true(is_call(~foo(bar), "foo", n = 1))
  expect_false(is_call(~foo(bar), "foo", n = 2))

  expect_true(is_call(~foo::bar()), quote(foo::bar()))

  expect_false(is_call(~1))
  expect_false(is_call(~NULL))

  expect_true(is_unary_call(~ +3))
  expect_true(is_binary_call(~ 3 + 3))
})


# coercion ----------------------------------------------------------------

test_that("as_name() produces names", {
  expect_equal(as_name("a"), quote(a))
  expect_equal(as_name(quote(a)), quote(a))
  expect_equal(as_name(quote(a())), quote(a))
  expect_equal(as_name(~ a), quote(a))
  expect_equal(as_name(~ a()), quote(a))

  expect_error(as_name(c("a", "b")), "Cannot parse character vector of length > 1")
})

test_that("as_call() produces calls", {
  expect_equal(as_call(quote(a)), quote(a()))
  expect_equal(as_call(quote(a())), quote(a()))
  expect_equal(as_call("a()"), quote(a()))
  expect_equal(as_call(~ a()), quote(a()))

  expect_error(as_call(c("a", "b")), "Cannot parse character vector of length > 1")
})

test_that("as_name() handles prefixed call names", {
  expect_identical(as_name(quote(foo::bar())), quote(foo::bar))
  expect_identical(as_name(~foo@bar()), quote(foo@bar))
})
