context("vec-squash")

# Squashing ----------------------------------------------------------

test_that("vectors and names are squashed", {
  expect_identical(
    squash_dbl(list(a = 1e0, list(c(b = 2e1, c = 3e1), d = 4e1, list(5e2, list(e = 6e3, c(f = 7e3)))), 8e0)),
    c(a = 1e0, b = 2e1, c = 3e1, d = 4e1, 5e2, e = 6e3, f = 7e3, 8e0)
  )
})

test_that("bad outer names warn even at depth", {
  expect_warning(regex = "Outer names",
    expect_identical(squash_dbl(list(list(list(A = c(a = 1))))), c(a = 1))
  )
})

test_that("lists are squashed", {
  expect_identical(squash(list(a = 1e0, list(c(b = 2e1, c = 3e1), d = 4e1, list(5e2, list(e = 6e3, c(f = 7e3)))), 8e0)), list(a = 1, c(b = 20, c = 30), d = 40, 500, e = 6000, c(f = 7000), 8))
})

test_that("squash_if() handles custom predicate", {
  is_foo <- function(x) inherits(x, "foo") || is_bare_list(x)
  foo <- structure(list("bar"), class = "foo")
  x <- list(1, list(foo, list(foo, 100)))
  expect_identical(squash_if(x, is_foo), list(1, "bar", "bar", 100))
})


# Flattening ---------------------------------------------------------

test_that("vectors and names are flattened", {
  expect_identical(flatten_dbl(list(a = 1, c(b = 2), 3)), c(a = 1, b = 2, 3))
  expect_identical(flatten_dbl(list(list(a = 1), list(c(b = 2)), 3)), c(a = 1, b = 2, 3))
  expect_error(flatten_dbl(list(1, list(list(2)), 3)), "Can't convert")
})

test_that("bad outer names warn when flattening", {
  expect_warning(expect_identical(flatten_dbl(list(a = c(A = 1))), c(A = 1)), "Outer names")
  expect_warning(expect_identical(flatten_dbl(list(a = 1, list(b = c(B = 2)))), c(a = 1, B = 2)), "Outer names")
})

test_that("lists are flattened", {
  x <- list(1, list(2, list(3, list(4))))
  expect_identical(flatten(x), list(1, 2, list(3, list(4))))
  expect_identical(flatten(flatten(x)), list(1, 2, 3, list(4)))
  expect_identical(flatten(flatten(flatten(x))), list(1, 2, 3, 4))
  expect_identical(flatten(flatten(flatten(flatten(x)))), list(1, 2, 3, 4))
})

test_that("flatten() checks type of splice box contents and coerces to list", {
  expect_identical(flatten(list(1L, splice(2:3))), list(1L, 2L, 3L))
})

test_that("is_spliced_bare() is TRUE for bare lists", {
  expect_true(is_spliced_bare(list()))
})

test_that("flatten_if() handles custom predicate", {
  obj <- structure(list(1:2), class = "foo")
  x <- list(obj, splice(obj), unclass(obj))

  expect_identical(flatten_if(x), list(obj, obj[[1]], unclass(obj)))
  expect_identical(flatten_if(x, is_bare_list), list(obj, splice(obj), obj[[1]]))

  pred <- function(x) is_bare_list(x) || is_spliced(x)
  expect_identical(flatten_if(x, pred), list(obj, obj[[1]], obj[[1]]))
})

test_that("flatten_if() handles external pointers", {
  obj <- structure(list(1:2), class = "foo")
  x <- list(obj, splice(obj), unclass(obj))

  expect_identical(flatten_if(x, rlang_test_is_spliceable), list(obj[[1]], splice(obj), unclass(obj)))

  ptr <- rlang_test_is_spliceable[[1]]
  expect_identical(flatten_if(x, ptr), list(obj[[1]], splice(obj), unclass(obj)))

  expect_is(rlang_test_is_spliceable, "fn_pointer")
})

test_that("flatten() splices names", {
  expect_warning(regexp = "Outer names",
    expect_identical(
      flatten(list(a = list(A = TRUE), b = list(B = FALSE))) ,
      list(A = TRUE, B = FALSE)
    )
  )
  expect_warning(regexp = "Outer names",
    expect_identical(
      flatten(list(a = list(TRUE), b = list(FALSE))) ,
      list(TRUE, FALSE)
    )
  )
})

test_that("typed flatten return typed vectors", {
  x <- list(list(TRUE), list(FALSE))
  expect_identical(flatten_lgl(x), lgl(TRUE, FALSE))
  expect_identical(flatten_int(x), int(TRUE, FALSE))
  expect_identical(flatten_dbl(x), dbl(TRUE, FALSE))
  expect_identical(flatten_cpl(x), cpl(TRUE, FALSE))

  x <- list(list("foo"), list("bar"))
  expect_identical(flatten_chr(x), chr("foo", "bar"))

  x <- list(bytes(0L), bytes(1L))
  expect_identical(flatten_raw(x), as.raw(0:1))
})

test_that("typed squash return typed vectors", {
  x <- list(list(list(TRUE)), list(list(FALSE)))
  expect_identical(squash_lgl(x), lgl(TRUE, FALSE))
  expect_identical(squash_int(x), int(TRUE, FALSE))
  expect_identical(squash_dbl(x), dbl(TRUE, FALSE))
  expect_identical(squash_cpl(x), cpl(TRUE, FALSE))

  x <- list(list(list("foo")), list(list("bar")))
  expect_identical(squash_chr(x), chr("foo", "bar"))

  x <- list(list(bytes(0L)), list(bytes(1L)))
  expect_identical(squash_raw(x), as.raw(0:1))
})

test_that("flatten_if() and squash_if() handle primitive functions", {
  expect_identical(flatten_if(list(list(1), 2), is.list), list(1, 2))
  expect_identical(squash_if(list(list(list(1)), 2), is.list), list(1, 2))
})
