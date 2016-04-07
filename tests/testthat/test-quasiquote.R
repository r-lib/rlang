context("quasiquote")

test_that("checks types of input", {
  expect_error(quasiquote_(quote(x + 1), 10), "must be an environment")
})

test_that("evaluates contents of uq()", {
  expect_equal(quasiquote_(quote(uq(1 + 2))), 3)
})

test_that("contents of uqs() must be a vector", {
  expr <- quote(1 + uqs(environment()))

  expect_error(quasiquote_(expr), "`x` must be a vector")
})

test_that("values of uqs() spliced into expression", {
  expr <- quote(f(a, uqs(list(quote(b), quote(c))), d))
  expect_identical(quasiquote_(expr), quote(f(a, b, c, d)))
})

test_that("names within uqs() are preseved", {
  expr <- quote(f(uqs(list(a = quote(b)))))
  expect_identical(quasiquote_(expr), quote(f(a = b)))
})
