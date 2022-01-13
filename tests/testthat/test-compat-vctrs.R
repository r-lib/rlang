test_that("data_frame() recycles", {
  expect_equal(
    data_frame(x = 1, y = 1:2),
    data_frame(x = c(1, 1), y = 1:2)
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
  expect_equal(
    vec_slice(fct, c(1, 3)),
    fct_exp
  )
  expect_equal(
    vec_init(fct, 2),
    factor(c(NA, NA), c("a", "b"))
  )

  df <- data_frame(
    x = fct,
    y = data_frame(a = list(1, 2, 3))
  )
  df_exp <- data_frame(
    x = fct_exp,
    y = data_frame(a = list(1, 3))
  )
  expect_equal(
    vec_slice(df, c(1, 3)),
    df_exp
  )
  expect_equal(
    vec_init(df, 2),
    data_frame(
      x = vec_init(fct, 2),
      y = data_frame(a = list(NULL, NULL))
    )
  )

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

test_that("vec_assign() works", {
  expect_identical(
    vec_assign(1:2, 1, FALSE),
    c(0L, 2L)
  )
  expect_error(
    vec_assign(1:2, 1, 1.5),
    "Can't convert"
  )

  df <- data_frame(x = list(1, 2), y = data_frame(a = c("a", "b")))

  expect_equal(
    vec_assign(df, 2, data_frame(x = list(10))),
    data_frame(x = list(1, 10), y = data_frame(a = c("a", NA)))
  )
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

test_that("vec_ptype_common() works", {
  expect_equal(
    vec_ptype_common(list(lgl(), dbl(), NA)),
    dbl()
  )

  expect_snapshot(
    error = TRUE,
    vec_ptype_common(list(lgl(), dbl(), ""))
  )
})

test_that("vec_ptype_common() finalises unspecified type", {
  expect_equal(
    vec_ptype_common(list(NA, NA)),
    logical()
  )
})

test_that("safe casts work", {
  expect_equal(vec_cast(NULL, logical()), NULL)
  expect_equal(vec_cast(TRUE, logical()), TRUE)
  expect_equal(vec_cast(1L, logical()), TRUE)
  expect_equal(vec_cast(1, logical()), TRUE)

  expect_equal(vec_cast(NULL, integer()), NULL)
  expect_equal(vec_cast(TRUE, integer()), 1L)
  expect_equal(vec_cast(1L, integer()), 1L)
  expect_equal(vec_cast(1, integer()), 1L)

  expect_equal(vec_cast(NULL, double()), NULL)
  expect_equal(vec_cast(TRUE, double()), 1L)
  expect_equal(vec_cast(1.5, double()), 1.5)
  expect_equal(vec_cast(1.5, double()), 1.5)

  expect_equal(vec_cast("", chr()), "")

  expect_equal(vec_cast(NULL, character()), NULL)
  expect_equal(vec_cast(NA, character()), NA_character_)

  expect_equal(vec_cast(NULL, list()), NULL)
  expect_equal(vec_cast(NA, list()), list(NULL))
  expect_equal(vec_cast(list(1L, 2L), list()), list(1L, 2L))
})

test_that("lossy casts throw", {
  expect_error(vec_cast(c(2L, 1L), logical()), "convert")
  expect_error(vec_cast(c(2, 1), logical()), "convert")

  expect_error(vec_cast(c(2.5, 2), integer()), "convert")

  expect_snapshot(vec_cast(1.5, 2L), error = TRUE)
})

test_that("invalid casts throw", {
  expect_error(vec_cast(c("x", "TRUE"), logical()), "convert")
  expect_error(vec_cast(list(c(TRUE, FALSE), TRUE), logical()), "convert")
  expect_error(vec_cast(factor("a"), logical()), "Unimplemented")

  expect_error(vec_cast(factor("a"), integer()), "Unimplemented")
  expect_error(vec_cast("1", integer()), "convert")
  expect_error(vec_cast(list(1L), integer()), "convert")

  expect_error(vec_cast("1.5", double()), "convert")

  expect_error(vec_cast(TRUE, character()), "convert")
  expect_error(vec_cast(list("x"), character()), "convert")

  expect_error(vec_cast(1:2, list()), "convert")
})

test_that("vec_cast_common(): empty input returns list()", {
  expect_equal(vec_cast_common(list()), list())
  expect_equal(vec_cast_common(list(NULL, NULL)), list(NULL, NULL))
})

test_that("data frames have a common type", {
  exp <- data.frame(x = dbl(), y = chr())
  exp_rlib_df <- new_data_frame(exp, .class = "tbl")
  exp_tibble <- new_data_frame(exp, .class = c("tbl_df", "tbl"))

  expect_equal(
    vec_ptype2(data.frame(x = 1, y = ""), data.frame(y = "")),
    exp
  )
  expect_equal(
    vec_ptype2(data_frame(x = 1, y = ""), data_frame(y = "")),
    exp_rlib_df
  )
  expect_equal(
    vec_ptype2(data_frame(x = 1, y = ""), data.frame(y = "")),
    exp_rlib_df
  )

  expect_error(
    vec_ptype2(data.frame(x = 1, y = ""), data.frame(y = 1)),
    "combine"
  )

  skip_if_not_installed("tibble")
  expect_equal(
    vec_ptype2(data_frame(x = 1, y = ""), tibble::tibble(y = "")),
    exp_tibble
  )
  expect_equal(
    vec_ptype2(tibble::tibble(x = 1, y = ""), data.frame(y = "")),
    exp_tibble
  )
})

test_that("data frame takes max of individual variables", {
  dt1 <- data.frame(x = FALSE, y = 1L)
  dt2 <- data.frame(x = 1.5, y = 1.5)

  expect_equal(
    vec_ptype_common(list(dt1, dt2)),
    vec_ptype_common(list(dt2))
  )
})

test_that("data frame combines variables", {
  dt1 <- data.frame(x = 1)
  dt2 <- data.frame(y = 1)

  expect_equal(
    vec_ptype_common(list(dt1, dt2)),
    vec_ptype_common(list(data.frame(x = double(), y = double())))
  )
})

test_that("can cast data frames", {
  expect_equal(
    vec_cast(data.frame(y = ""), data.frame(x = 1, y = "")),
    data.frame(x = na_dbl, y = "")
  )

  expect_equal(
    vec_cast(data.frame(y = ""), data_frame(x = 1, y = "")),
    data_frame(x = na_dbl, y = "")
  )

  skip_if_not_installed("tibble")
  expect_equal(
    vec_cast(data.frame(y = ""), tibble::tibble(x = 1, y = "")),
    tibble::tibble(x = na_dbl, y = "")
  )
})

test_that("can bind data frames", {
  expect_equal(
    vec_rbind(
      data.frame(x = 1),
      data_frame(y = "")
    ),
    data_frame(x = c(1, NA), y = c(NA, ""))
  )

  expect_equal(
    vec_cbind(
      data_frame(x = data_frame(a = TRUE)),
      data_frame(y = list(""))
    ),
    data_frame(x = data_frame(a = TRUE), y = list(""))
  )

  expect_equal(
    vec_rbind(
      data_frame(x = TRUE),
      data_frame(y = list(""))
    ),
    data_frame(x = c(TRUE, NA), y = list(NULL, ""))
  )

  # `rbind()` has trouble binding df-cols on old R versions
  skip_if(getRversion() < "4.0")
  expect_equal(
    vec_rbind(
      data_frame(x = data_frame(a = TRUE)),
      data_frame(y = list(""))
    ),
    data_frame(x = data_frame(a = c(TRUE, NA)), y = list(NULL, ""))
  )
})

test_that("casting to df type uses same column order", {
  df1 <- data.frame(x = 1, y = 2)
  df2 <- data.frame(y = 3, x = 4)
  expect_equal(
    vec_cast_common(list(df1, df2)),
    list(df1, df2[2:1])
  )

  df1 <- data.frame(y = 2)
  df2 <- data.frame(y = 3, x = 4)
  expect_equal(
    vec_cast_common(list(df1, df2)),
    list(data.frame(y = 2, x = na_dbl), df2)
  )
})

test_that("vec_as_location() works", {
  n <- 4
  names <- letters[1:4]

  i <- c(2, 3)
  expect_identical(vec_as_location(i, n, names), 2:3)

  i <- -c(2, 3)
  expect_identical(vec_as_location(i, n, names), c(1L, 4L))

  i <- c(TRUE, FALSE, TRUE, FALSE)
  expect_identical(vec_as_location(i, n, names), c(1L, 3L))

  i <- c("a", "d")
  expect_identical(vec_as_location(i, n, names), c(1L, 4L))
})

test_that("vec_as_location() recycles scalar logical inputs", {
  expect_equal(vec_as_location(TRUE, 0), int())
  expect_equal(vec_as_location(FALSE, 0), int())
})

test_that("vec_slice() preserves attributes of data frames", {
  df <- data_frame(x = 1:2)
  attr(df, "foo") <- TRUE

  out <- vec_slice(df, 1)
  expect_true(attr(out, "foo"))
})

test_that("vec_slice() doesn't restore attributes if there is a `[` method", {
  df <- new_data_frame(
    df_list(x = 1:2),
    .class = "rlang_foobar",
    foo = "bar"
  )
  local_methods(`[.rlang_foobar` = function(x, ...) {
    out <- NextMethod()
    attr(out, "foo") <- "dispatched"
    out
  })

  expect_equal(
    attr(vec_slice(df, 1), "foo"),
    "dispatched"
  )
})

test_that("vec_slice() preserves attributes of vectors", {
  x <- set_names(1:2, c("a", "b"))
  attr(x, "foo") <- TRUE

  out <- vec_slice(x, 1)
  expect_true(attr(out, "foo"))
  expect_equal(attr(out, "names"), "a")
})

test_that("can row-bind unspecified columns", {
  expect_equal(
    vec_rbind(
      data_frame(x = NA),
      data_frame(x = "")
    ),
    data_frame(x = c(NA, ""))
  )
})

test_that("unspecified is detected recursively", {
  ptype <- vec_ptype(data_frame(x = NA))
  expect_s3_class(ptype$x, "rlang_unspecified")
})

test_that("ptype is finalised", {
  x <- data_frame(x = NA)

  out <- vec_cast_common(list(x, x))[[1]]
  expect_identical(out$x, NA)

  out <- vec_cast_common(list(out, x))[[1]]
  expect_identical(out$x, NA)
})
