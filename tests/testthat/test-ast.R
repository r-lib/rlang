context("ast")

test_that("common cases are as expected", {
  x <- list(
    1,
    quote(x),
    quote(a + b),
    quote(function(x = 1, y = a + b) {
      c + d
    })
  )

  expect_output_file(ast_(x), "ast-sample.txt", update = TRUE)
})
