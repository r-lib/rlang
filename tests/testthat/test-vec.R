context("vec")

test_that("can poke a range to a vector", {
  y <- 11:15
  x <- 1:5
  x_addr <- sexp_address(x)

  expect_error(vec_poke_range(x, 2L, y, 2L, 6L), "too small")

  vec_poke_range(x, 2L, y, 2L, 4L)
  expect_identical(x, int(1L, 12:14L, 5L))
  expect_identical(x_addr, sexp_address(x))
})

test_that("can poke `n` elements to a vector", {
  y <- 11:15
  x <- 1:5
  x_addr <- sexp_address(x)

  expect_error(vec_poke_n(x, 2L, y, 2L, 5L), "too small")

  vec_poke_n(x, 2L, y, 2L, 4L)
  expect_identical(x, int(1L, 12:15))
  expect_identical(x_addr, sexp_address(x))
})

test_that("can poke to a vector with default parameters", {
  y <- 11:15
  x <- 1:5
  x_addr <- sexp_address(x)

  vec_poke_range(x, 1L, y)
  expect_identical(x, y)
  expect_identical(x_addr, sexp_address(x))

  x <- 1:5
  x_addr <- sexp_address(x)

  vec_poke_n(x, 1L, y)
  expect_identical(x, y)
  expect_identical(x_addr, sexp_address(x))
})

test_that("can poke to a vector with double parameters", {
  y <- 11:15
  x <- 1:5
  x_addr <- sexp_address(x)

  vec_poke_range(x, 2, y, 2, 5)
  expect_identical(x, int(1L, 12:15L))
  expect_identical(x_addr, sexp_address(x))

  y <- 11:15
  x <- 1:5
  x_addr <- sexp_address(x)

  vec_poke_n(x, 2, y, 2, 4)
  expect_identical(x, int(1L, 12:15))
  expect_identical(x_addr, sexp_address(x))
})

test_that("vector pokers fail if parameters are not integerish", {
  y <- 11:15
  x <- 1:5

  expect_error(vec_poke_n(x, 2.5, y, 2L, 5L), "integerish")
  expect_error(vec_poke_n(x, 2L, y, 2.5, 5L), "integerish")
  expect_error(vec_poke_n(x, 2L, y, 2L, 5.5), "integerish")

  expect_error(vec_poke_range(x, 2.5, y, 2L, 4L), "integerish")
  expect_error(vec_poke_range(x, 2L, y, 2.5, 4L), "integerish")
  expect_error(vec_poke_range(x, 2L, y, 2L, 4.5), "integerish")
})

test_that("is_string() returns FALSE for `NA`", {
  expect_false(is_string(na_chr))
})

test_that("are_na() requires vector input but not is_na()", {
  expect_error(are_na(base::eval), "must be an atomic vector")
  expect_false(is_na(base::eval))
})

test_that("are_na() fails with lists (#558)", {
  expect_error(are_na(mtcars), "must be an atomic vector")
})
