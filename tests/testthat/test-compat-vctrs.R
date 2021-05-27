test_that("data_frame() recycles", {
  expect_equal(
    data_frame(x = 1, y = 1:2),
    data.frame(x = c(1, 1), y = 1:2)
  )

  out <- data_frame(x = list(1), y = 1:2)
  expect_equal(out$x, list(1, 1))
})

test_that("data_frame() nests", {
  df <- data_frame(a = 3:4)
  out <- data_frame(x = 1:2, y = df)
  expect_equal(out$y, df)

  out <- data_frame(x = 1:2, y = data_frame(a = 1))
  expect_equal(out$y, data_frame(a = c(1, 1)))
})

test_that("new_data_frame handles zero-length inputs", {
  # Zero-length input creates zero-length data frame
  d <- data_frame(x = numeric(0), y = numeric(0))
  expect_equal(nrow(d), 0L)

  # Constants are ignored in the context of zero-length input
  d <- data_frame(x = numeric(0), y = numeric(0), z = 1)
  expect_equal(nrow(d), 0L)

  # Vectors of length > 1 don't mix with zero-length input
  expect_error(
    data_frame(x = numeric(0), y = numeric(0), z = 1, a = c(1, 2)),
    "Inputs can't be recycled"
  )

  # Explicit recycling doesn't work with zero-length input
  expect_error(
    new_data_frame(df_list(x = numeric(0), z = 1, .size = 5)),
    "Inputs can't be recycled to `size`."
  )

  # But it works without
  d <- new_data_frame(df_list(x = 1, y = "a", .size = 3))
  expect_equal(nrow(d), 3L)
  expect_identical(d$x, rep(1, 3L))
  expect_identical(d$y, rep("a", 3L))

  # Can supply size for empty df
  d <- new_data_frame(.size = 3)
  expect_equal(dim(d), c(3, 0))
})

test_that("can slice vectors and data frames", {
  fct <- factor(c("a", "b", "a"))
  fct_exp <- factor(c("a", "a"), levels = c("a", "b"))
  expect_equal(vec_slice(fct, c(1, 3)), fct_exp)

  df <- data_frame(
    x = fct,
    y = data_frame(a = list(1, 2, 3))
  )
  df_exp <- data_frame(
    x = fct_exp,
    y = data_frame(a = list(1, 3))
  )
  expect_equal(vec_slice(df, c(1, 3)), df_exp)

  rep_exp <- data_frame(
    x = rep(fct, 2),
    y = data_frame(a = rep(list(1, 2, 3), 2))
  )
  expect_equal(vec_rep(df, 2), rep_exp)
})

test_that("vec_slice() is generic", {
  skip_if_not_installed("tibble")
  tib <- tibble::tibble(x = 1:2, y = data_frame(a = 3:4))
  expect_equal(vec_slice(tib, 1), tib[1, ])
})

test_that("vec_ptype2() implements base coercions", {
  expect_equal(vec_ptype2(lgl(), lgl()), lgl())
  expect_equal(vec_ptype2(lgl(), int()), int())
  expect_equal(vec_ptype2(lgl(), dbl()), dbl())
  expect_error(vec_ptype2(lgl(), chr()))
  expect_error(vec_ptype2(lgl(), list()))
  expect_error(vec_ptype2(lgl(), raw()))

  expect_equal(vec_ptype2(int(), lgl()), int())
  expect_equal(vec_ptype2(int(), int()), int())
  expect_equal(vec_ptype2(int(), dbl()), int())
  expect_error(vec_ptype2(int(), chr()))
  expect_error(vec_ptype2(int(), list()))
  expect_error(vec_ptype2(int(), raw()))

  expect_equal(vec_ptype2(dbl(), lgl()), dbl())
  expect_equal(vec_ptype2(dbl(), int()), dbl())
  expect_equal(vec_ptype2(dbl(), dbl()), dbl())
  expect_error(vec_ptype2(dbl(), chr()))
  expect_error(vec_ptype2(dbl(), list()))
  expect_error(vec_ptype2(dbl(), raw()))

  expect_equal(vec_ptype2(chr(), chr()), chr())
  expect_error(vec_ptype2(chr(), lgl()))
  expect_error(vec_ptype2(chr(), int()))
  expect_error(vec_ptype2(chr(), dbl()))
  expect_error(vec_ptype2(chr(), list()))
  expect_error(vec_ptype2(chr(), raw()))

  expect_equal(vec_ptype2(list(), list()), list())
  expect_error(vec_ptype2(list(), lgl()))
  expect_error(vec_ptype2(list(), int()))
  expect_error(vec_ptype2(list(), dbl()))
  expect_error(vec_ptype2(list(), chr()))
  expect_error(vec_ptype2(list(), raw()))

  expect_snapshot(vec_ptype2(lgl(), chr()), error = TRUE)
  expect_snapshot(vec_ptype2(factor("a"), lgl()), error = TRUE)
})

test_that("vec_ptype2() deals with unspecified vectors", {
  expect_equal(vec_ptype2(NA, NA), .rlang_vctrs_unspecified())
  expect_equal(vec_ptype2(NA, lgl()), lgl())
  expect_equal(vec_ptype2(NA, int()), int())
  expect_equal(vec_ptype2(NA, dbl()), dbl())
  expect_equal(vec_ptype2(NA, chr()), chr())
  expect_equal(vec_ptype2(NA, list()), list())
  expect_equal(vec_ptype2(lgl(), NA), lgl())
  expect_equal(vec_ptype2(int(), NA), int())
  expect_equal(vec_ptype2(dbl(), NA), dbl())
  expect_equal(vec_ptype2(chr(), NA), chr())
  expect_equal(vec_ptype2(list(), NA), list())
})

test_that("vec_is_unspecified() knows about empty logicals", {
  expect_true(vec_is_unspecified(NA))
  expect_false(vec_is_unspecified(lgl()))
})
