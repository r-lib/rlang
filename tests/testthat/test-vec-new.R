context("vec-new")

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
  expect_identical(list2(), list())
})

test_that("objects are not spliced", {
  expect_error(lgl(structure(list(TRUE, TRUE), class = "bam")), "Can't splice S3 objects")
})

test_that("explicitly spliced lists are spliced", {
  expect_identical(lgl(FALSE, splice(list(TRUE, TRUE))), c(FALSE, TRUE, TRUE))
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

test_that("list2() doesn't splice bare lists", {
  expect_identical(list2(list(1, 2)), list(list(1, 2)))
  expect_identical(list2(!!! list(1, 2)), list(1, 2))
})

test_that("atomic inputs are implicitly coerced", {
  expect_identical(lgl(10L, FALSE, list(TRUE, 0L, 0)), c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(dbl(10L, 10, TRUE, list(10L, 0, TRUE)), c(10, 10, 1, 10, 0, 1))

  expect_error(lgl("foo"), "Can't convert a character vector to a logical vector")
  expect_error(chr(10), "Can't convert a double vector to a character vector")
})

test_that("type errors are handled", {
  expect_error(lgl(env(a = 1)), "Internal error: expected a vector")
  expect_error(lgl(list(env())), "Internal error: expected a vector")
})

test_that("empty inputs are spliced", {
  expect_identical(lgl(NULL, lgl(), list(NULL, lgl())), lgl())
  expect_warning(regexp = NA, expect_identical(lgl(a = NULL, a = lgl(), list(a = NULL, a = lgl())), lgl()))
})

test_that("list2() splices names", {
  expect_identical(list2(a = TRUE, b = FALSE), list(a = TRUE, b = FALSE))
  expect_identical(list2(c(A = TRUE), c(B = FALSE)), list(c(A = TRUE), c(B = FALSE)))
  expect_identical(list2(a = c(A = TRUE), b = c(B = FALSE)), list(a = c(A = TRUE), b = c(B = FALSE)))
})

test_that("ll() is an alias to list2()", {
  expect_identical(ll(!!! list(1, 2)), list(1, 2))
})

test_that("vector ctors take names arguments", {
  expect_identical(new_logical(2, letters[1:2]), c(a = NA, b = NA))
  expect_identical(new_integer(2, letters[1:2]), c(a = na_int, b = na_int))
  expect_identical(new_double(2, letters[1:2]), c(a = na_dbl, b = na_dbl))
  expect_identical(new_complex(2, letters[1:2]), c(a = na_cpl, b = na_cpl))
  expect_identical(new_character(2, letters[1:2]), c(a = na_chr, b = na_chr))
  expect_identical(new_raw(2, letters[1:2]), set_names(raw(2), c("a", "b")))
  expect_identical(new_list(2, letters[1:2]), list(a = NULL, b = NULL))
})

test_that("rep_named() repeats along names", {
  expect_error(rep_named(1, list(1)), "must be `NULL` or a character vector")
  expect_identical(rep_named(NULL, list(1)), named_list())
  expect_identical(rep_named(chr(), list(1)), named_list())
  expect_identical(rep_named(c("foo", "bar"), list(1)), list(foo = 1, bar = 1))
})

test_that("rep_along() reps along vector", {
  expect_identical(rep_along(1:2, list(zap())), list(zap(), zap()))
})

test_that("chr() supports logical NA", {
  expect_identical(chr(NA), na_chr)
  expect_identical(chr(NA, NA), c(na_chr, na_chr))
})
