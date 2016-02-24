context("quasiquote")

test_that("checks types of input", {
  expect_error(quasiquote_(environment(), 10), "must be a call")
  expect_error(quasiquote_(10, 10), "must be an environment")
})

test_that("evaluates contents of (())", {
  expect_equal(quasiquote_(quote((( 1 + 2)))), 3)
})

test_that("doesn't evaluate contents of (", {
  expect_identical(quasiquote_(quote(( 1 + 2))), quote((1 + 2)))
})

test_that("preserves code outside of (())", {
  expect_identical(quasiquote_(quote(1 + 2)), quote(1 + 2))
})

test_that("mixes the two", {
  expect_identical(quasiquote_(quote(1 + ((1 + 1)))), quote(1 + 2))
})

test_that("contents of ({}) must be a list", {
  expr <- quote(1 + ({2}))

  expect_error(quasiquote_(expr), "must evaluate to a list")
})

test_that("values of ({}) spliced into expression", {
  expr <- quote(f(a, ({ list(quote(b), quote(c)) }), d))
  expect_identical(quasiquote_(expr), quote(f(a, b, c, d)))
})

test_that("names within  ({}) are preseved expression", {
  expr <- quote(f(({ list(a = quote(b)) })))
  expect_identical(quasiquote_(expr), quote(f(a = b)))
})

# unquote/unquote_splice detection -------------------------------------------------------

test_that("unquote detects paired parens", {
  # Obvious negatives
  expect_false(is_unquote(10))
  expect_false(is_unquote(x))
  expect_false(is_unquote(f()))

  # Pathological
  expect_false(is_unquote(`(`()))
  expect_false(is_unquote(`(`(1, 2)))

  # Nearly correct
  out <- is_unquote( (x) )
  expect_false(out)

  quote(`~`(1, 2, 3))

  out <- is_unquote( ({x}) )
  expect_false(out)

  # Correct
  out <- is_unquote( ((x)) )
  expect_true(out)
})

test_that("unquote_splice detects ( + {", {
  # Obvious negatives
  expect_false(is_unquote_splice(10))
  expect_false(is_unquote_splice(x))
  expect_false(is_unquote_splice(f()))

  # Pathological
  expect_false(is_unquote_splice(`(`()))
  expect_false(is_unquote_splice(`(`(1, 2)))

  # Nearly correct
  out <- is_unquote_splice( (x) )
  expect_false(out)

  out <- is_unquote_splice( ((x)) )
  expect_false(out)

  # Correct
  out <- is_unquote_splice( ({x}) )
  expect_true(out)
})
