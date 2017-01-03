context("operators")

test_that("%|% returns default value", {
  lgl <- c(TRUE, TRUE, NA, FALSE) %|% FALSE
  expect_identical(lgl, c(TRUE, TRUE, FALSE, FALSE))

  int <- c(1L, 2L, NA, 4L) %|% 3L
  expect_identical(int, 1:4)

  dbl <- c(1, 2, NA, 4) %|% 3
  expect_identical(dbl, as.double(1:4))

  chr <- c("1", "2", NA, "4") %|% "3"
  expect_identical(chr, as.character(1:4))

  cpx <- c(1i, 2i, NA, 4i) %|% 3i
  expect_equal(cpx, c(1i, 2i, 3i, 4i))
})

test_that("%|% fails with wrong types", {
  expect_error(c(1L, NA) %|% 2)
  expect_error(c(1, NA) %|% "")
})

