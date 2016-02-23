context("quasiquote")

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
