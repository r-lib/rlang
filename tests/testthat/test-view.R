test_that("views can be created", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  expect_identical(x, base[2:4])

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  expect_identical(x, base[2:4])

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  expect_identical(x, base[2:4])

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  expect_identical(x, base[2:4])
})

test_that("views have right length", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(length(x), 3L)

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(length(x), 3L)

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(length(x), 3L)

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(length(x), 3L)
})

test_that("views can be sliced with subset", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[1:2], base[2:3])
  expect_identical(x[2:4], base[3:5])
  expect_identical(x[0L], base[0L])
  expect_identical(x[c(0L, 2L)], base[c(0L, 3L)])
  expect_false(view_is_materialized(x))

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[1:2], base[2:3])
  expect_identical(x[2:4], base[3:5])
  expect_identical(x[0L], base[0L])
  expect_identical(x[c(0L, 2L)], base[c(0L, 3L)])
  expect_false(view_is_materialized(x))

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[1:2], base[2:3])
  expect_identical(x[2:4], base[3:5])
  expect_identical(x[0L], base[0L])
  expect_identical(x[c(0L, 2L)], base[c(0L, 3L)])
  expect_false(view_is_materialized(x))

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[1:2], base[2:3])
  expect_identical(x[2:4], base[3:5])
  expect_identical(x[0L], base[0L])
  expect_identical(x[c(0L, 2L)], base[c(0L, 3L)])
  expect_false(view_is_materialized(x))
})

test_that("views can be sliced with subset2", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))
})

test_that("views can be manually materialized", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))
})

test_that("view duplication causes a materialization", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)
})

test_that("can make zero length view", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)
})
