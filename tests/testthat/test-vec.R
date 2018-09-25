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

test_that("seq2() does empty punning on its arguments", {
  expect_identical(seq2(int(), 1), int())
  expect_identical(seq2(0, int()), int())
})

test_that("seq2() fails when arguments are length > 1", {
  expect_error(seq2(1, 1:2), "must be length")
  expect_error(seq2(1:2, 1), "must be length")
})

test_that("match2() returns empty vector on failed match", {
  expect_identical(match2(1:3, 2), 2L)
  expect_identical(match2(1:3, 10), int())
})
