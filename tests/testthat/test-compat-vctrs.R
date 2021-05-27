test_that("data_frame() recycles", {
  expect_equal(
    data_frame(x = 1, y = 1:2),
    data.frame(x = c(1, 1), y = 1:2)
  )

  out <- data_frame(x = list(1), y = 1:2)
  expect_equal(out$x, list(1, 1))
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
