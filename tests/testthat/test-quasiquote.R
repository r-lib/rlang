context("quasiquote")

test_that("evaluates contents of (())", {
  expect_equal(quasiquote_(quote((( 1 + 2)))), 3)
})

test_that("preserves code outside of (())", {
  expect_identical(quasiquote_(quote(1 + 2)), quote(1 + 2))
})

test_that("mixes the two", {
  expect_identical(quasiquote_(quote(1 + ((1 + 1)))), quote(1 + 2))
})

# unquote detection -------------------------------------------------------

test_that("unquote detects paired parens", {
  # Obvious negatives
  expect_false(is_unquote(10))
  expect_false(is_unquote(x))
  expect_false(is_unquote(f()))

  # Nearly correct
  out <- is_unquote( (x) )
  expect_false(out)

  out <- is_unquote( ({x}) )
  expect_false(out)

  # Correct
  out <- is_unquote( ((x)) )
  expect_true(out)
})
