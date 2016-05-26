context("ast")

test_that("common cases are as expected", {
  skip_on_cran() # because of unicode comparison problems.

  x <- list(
    1,
    quote(x),
    quote(a + b),
    quote(function(x = 1, y = a + b, z) {
      c + d
    })
  )

  expect_output_file(ast_(x), "ast-sample.txt", update = TRUE)
})


test_that("can print trees that can't be generated from text source", {
  skip_on_cran() # because of unicode comparison problems.

  x <- quote(foo())
  x[[2]] <- mtcars
  x[[3]] <- 1:10

  expect_output_file(ast_(x), "ast-irregular.txt", update = TRUE)
})
