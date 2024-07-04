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

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  expect_identical(x, base[2:4])

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  expect_identical(x, base[2:4])

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
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

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(length(x), 3L)

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(length(x), 3L)

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
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

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[1:2], base[2:3])
  expect_identical(x[2:4], base[3:5])
  expect_identical(x[0L], base[0L])
  expect_identical(x[c(0L, 2L)], base[c(0L, 3L)])
  expect_false(view_is_materialized(x))

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[1:2], base[2:3])
  expect_identical(x[2:4], base[3:5])
  expect_identical(x[0L], base[0L])
  expect_identical(x[c(0L, 2L)], base[c(0L, 3L)])
  expect_false(view_is_materialized(x))

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
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

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_identical(x[[1]], base[[2L]])
  expect_identical(x[[3]], base[[4L]])
  expect_error(x[[4]])
  expect_false(view_is_materialized(x))
})

test_that("views can be assigned to", {
  assign <- c(NA, TRUE)
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, c(TRUE, FALSE, TRUE, FALSE))

  assign <- c(NA, 5L)
  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, c(1L, 2L, 3L, 4L))

  assign <- c(NA, 5)
  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, c(1, 2, 3, 4))

  assign <- c(NA, 5) + 2i
  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, c(1, 2, 3, 4) + 1i)

  assign <- as.raw(c(0, 5))
  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, as.raw(c(1, 2, 3, 4)))

  assign <- c(NA, "e")
  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, c("a", "b", "c", "d"))

  skip_if_no_altlist()
  assign <- list(NA, "e")
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  x[2:3] <- assign
  expect_identical(x[2:3], assign)
  expect_identical(base, list("a", "b", "c", "d"))
})

test_that("views can wrap other views", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
  expect_false(view_is_materialized(x))

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
  expect_false(view_is_materialized(x))

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
  expect_false(view_is_materialized(x))

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
  expect_false(view_is_materialized(x))

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
  expect_false(view_is_materialized(x))

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
  expect_false(view_is_materialized(x))

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 1L, size = 3L)
  y <- vec_view(x, start = 2L, size = 2L)
  expect_identical(y[[2L]], base[[3L]])
  expect_identical(y[1:2], base[2:3])
  expect_identical(y, x[2:3])
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

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_false(view_is_materialized(x))
  materialized <- view_materialize(x)
  expect_true(view_is_materialized(x))
  expect_false(is_altrep(materialized))
})

test_that("views can be inspected", {
  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  expect_snapshot(view_inspect(x))
})

test_that("views can be roundtripped through serialization, and lose ALTREPness", {
  # This is nice because we don't have to worry about compatibility with
  # "old" view objects if we ever change the internals. It is also probably
  # just the correct way to do this.

  base <- c(TRUE, FALSE, TRUE, FALSE)
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])

  base <- c(1L, 2L, 3L, 4L)
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])

  base <- c(1, 2, 3, 4)
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])

  base <- c(1, 2, 3, 4) + 1i
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 3L)
  bytes <- serialize(x, connection = NULL)
  x <- unserialize(bytes)
  expect_false(is_altrep(x))
  expect_identical(x, base[2:4])
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

  base <- as.raw(c(1, 2, 3, 4))
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  base <- c("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)

  skip_if_no_altlist()
  base <- list("a", "b", "c", "d")
  x <- vec_view(base, start = 2L, size = 0L)
  expect_identical(x, base[0L])
  expect_identical(length(x), 0L)
})
