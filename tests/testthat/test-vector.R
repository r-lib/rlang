context("vector")

test_that("vector is modified", {
  x <- c(1, b = 2, c = 3, 4)
  out <- modify(x, 5, b = 20, splice(list(6, c = "30")))
  expect_equal(out, list(1, b = 20, c = "30", 4, 5, 6))
})

test_that("are_na() requires vector input but not is_na()", {
  expect_error(are_na(base::eval), "must be a vector")
  expect_false(is_na(base::eval))
})

test_that("atomic vectors are spliced", {
  lgl <- lgl(TRUE, c(TRUE, FALSE), list(FALSE, FALSE))
  expect_identical(lgl, c(TRUE, TRUE, FALSE, FALSE, FALSE))

  int <- int(1L, c(2L, 3L), list(4L, 5L))
  expect_identical(int, 1:5)

  dbl <- dbl(1, c(2, 3), list(4, 5))
  expect_identical(dbl, as_double(1:5))

  cpl <- cpl(1i, c(2i, 3i), list(4i, 5i))
  expect_identical(cpl, c(1i, 2i, 3i, 4i, 5i))

  chr <- chr("foo", c("foo", "bar"), list("buz", "baz"))
  expect_identical(chr, c("foo", "foo", "bar", "buz", "baz"))

  raw <- bytes(1, c(2, 3), list(4, 5))
  expect_identical(raw, bytes(1:5))
})

test_that("can create empty vectors", {
  expect_identical(lgl(), logical(0))
  expect_identical(int(), integer(0))
  expect_identical(dbl(), double(0))
  expect_identical(cpl(), complex(0))
  expect_identical(chr(), character(0))
  expect_identical(bytes(), raw(0))
  expect_identical(ll(), list())
})

test_that("objects are not spliced", {
  expect_error(lgl(structure(list(TRUE, TRUE), class = "bam")), "Can't splice S3 objects")
})

test_that("explicitly spliced lists are spliced", {
  expect_identical(lgl(FALSE, structure(list(TRUE, TRUE), class = "spliced")), c(FALSE, TRUE, TRUE))
})

test_that("splicing uses inner names", {
  expect_identical(lgl(c(a = TRUE, b = FALSE)), c(a = TRUE, b = FALSE))
  expect_identical(lgl(list(c(a = TRUE, b = FALSE))), c(a = TRUE, b = FALSE))
})

test_that("splicing uses outer names when scalar", {
  expect_identical(lgl(a = TRUE, b = FALSE), c(a = TRUE, b = FALSE))
  expect_identical(lgl(list(a = TRUE, b = FALSE)), c(a = TRUE, b = FALSE))
})

test_that("warn when outer names unless input is unnamed scalar atomic", {
  expect_warning(expect_identical(dbl(a = c(1, 2)), c(1, 2)), "Outer names")
  expect_warning(expect_identical(dbl(list(a = c(1, 2))), c(1, 2)), "Outer names")
  expect_warning(expect_identical(dbl(a = c(A = 1)), c(A = 1)), "Outer names")
  expect_warning(expect_identical(dbl(list(a = c(A = 1))), c(A = 1)), "Outer names")
})

test_that("warn when spliced lists have outer name", {
  expect_warning(lgl(list(c = c(cc = FALSE))), "Outer names")
})

test_that("ll() doesn't splice bare lists", {
  expect_identical(ll(list(1, 2)), list(list(1, 2)))
  expect_identical(ll(splice(list(1, 2))), list(1, 2))
})

test_that("atomic inputs are implicitly coerced", {
  expect_identical(lgl(10L, FALSE, list(TRUE, 0L, 0)), c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(dbl(10L, 10, TRUE, list(10L, 0, TRUE)), c(10, 10, 1, 10, 0, 1))

  expect_error(lgl("foo"), "Can't convert a string to a logical vector")
  expect_error(chr(10), "Can't convert a double vector to a character vector")
})

test_that("type errors are handled", {
  expect_error(lgl(get_env()), "Can't convert an environment to a logical vector")
  expect_error(lgl(list(get_env())), "Can't convert an environment to a logical vector")
})

test_that("empty inputs are spliced", {
  expect_identical(lgl(NULL, lgl(), list(NULL, lgl())), lgl())
  expect_warning(regexp = NA, expect_identical(lgl(a = NULL, a = lgl(), list(a = NULL, a = lgl())), lgl()))
})

test_that("ll() splices names", {
  expect_identical(ll(a = TRUE, b = FALSE), list(a = TRUE, b = FALSE))
  expect_identical(ll(c(A = TRUE), c(B = FALSE)), list(c(A = TRUE), c(B = FALSE)))
  expect_identical(ll(a = c(A = TRUE), b = c(B = FALSE)), list(a = c(A = TRUE), b = c(B = FALSE)))
})


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
  foo <- set_attrs(list("bar"), class = "foo")
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

test_that("flatten_if() handles custom predicate", {
  obj <- set_attrs(list(1:2), class = "foo")
  x <- list(obj, splice(obj), unclass(obj))

  expect_identical(flatten_if(x), list(obj, obj[[1]], unclass(obj)))
  expect_identical(flatten_if(x, is_bare_list), list(obj, splice(obj), obj[[1]]))

  pred <- function(x) is_bare_list(x) || is_spliced(x)
  expect_identical(flatten_if(x, pred), list(obj, obj[[1]], obj[[1]]))
})

test_that("flatten_if() handles external pointers", {
  obj <- set_attrs(list(1:2), class = "foo")
  x <- list(obj, splice(obj), unclass(obj))

  expect_identical(flatten_if(x, test_is_spliceable), list(obj[[1]], splice(obj), unclass(obj)))

  ptr <- test_is_spliceable[[1]]
  expect_identical(flatten_if(x, ptr), list(obj[[1]], splice(obj), unclass(obj)))

  expect_is(test_is_spliceable, "fn_pointer")
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
