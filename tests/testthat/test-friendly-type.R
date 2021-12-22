test_that("friendly_type_of() supports objects", {
  expect_equal(friendly_type_of(mtcars), "a <data.frame> object")
  expect_equal(friendly_type_of(quo(1)), "a <quosure> object")
})

test_that("friendly_type_of() supports matrices and arrays (#141)", {
  expect_equal(friendly_type_of(list()), "an empty list")
  expect_equal(friendly_type_of(matrix(list(1, 2))), "a list matrix")
  expect_equal(friendly_type_of(array(list(1, 2, 3), dim = 1:3)), "a list array")

  expect_equal(friendly_type_of(int()), "an empty integer vector")
  expect_equal(friendly_type_of(matrix(1:3)), "an integer matrix")
  expect_equal(friendly_type_of(array(1:3, dim = 1:3)), "an integer array")

  expect_equal(friendly_type_of(chr()), "an empty character vector")
  expect_equal(friendly_type_of(matrix(letters)), "a character matrix")
  expect_equal(friendly_type_of(array(letters[1:3], dim = 1:3)), "a character array")
})

test_that("friendly_type_of() supports missing arguments", {
  expect_equal(friendly_type_of(missing_arg()), "absent")
})

test_that("friendly_type_of() handles scalars", {
  expect_equal(friendly_type_of(NA), "`NA`")
  expect_equal(friendly_type_of(na_int), "an integer `NA`")
  expect_equal(friendly_type_of(na_dbl), "a numeric `NA`")
  expect_equal(friendly_type_of(na_cpl), "a complex `NA`")
  expect_equal(friendly_type_of(na_chr), "a character `NA`")

  expect_equal(friendly_type_of(TRUE), "`TRUE`")
  expect_equal(friendly_type_of(FALSE), "`FALSE`")

  expect_equal(friendly_type_of(1L), "an integer")
  expect_equal(friendly_type_of(1.0), "a number")
  expect_equal(friendly_type_of(1i), "a complex number")
  expect_equal(friendly_type_of(as.raw(1)), "a raw value")

  expect_equal(friendly_type_of("foo"), "a string")
  expect_equal(friendly_type_of(""), "`\"\"`")

  expect_equal(friendly_type_of(list(1)), "a list")

  expect_equal(friendly_type_of(matrix(NA)), "a logical matrix")
  expect_equal(friendly_type_of(matrix(1)), "a double matrix")
})

test_that("friendly_type_of() handles empty vectors", {
  expect_equal(friendly_type_of(lgl()), "an empty logical vector")
  expect_equal(friendly_type_of(int()), "an empty integer vector")
  expect_equal(friendly_type_of(dbl()), "an empty numeric vector")
  expect_equal(friendly_type_of(cpl()), "an empty complex vector")
  expect_equal(friendly_type_of(chr()), "an empty character vector")
  expect_equal(friendly_type_of(raw()), "an empty raw vector")
  expect_equal(friendly_type_of(list()), "an empty list")
})
