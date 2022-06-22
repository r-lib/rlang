test_that("obj_type_friendly() supports objects", {
  expect_equal(obj_type_friendly(mtcars), "a <data.frame> object")
  expect_equal(obj_type_friendly(quo(1)), "a <quosure> object")
})

test_that("obj_type_friendly() supports matrices and arrays (#141)", {
  expect_equal(obj_type_friendly(list()), "an empty list")
  expect_equal(obj_type_friendly(matrix(list(1, 2))), "a list matrix")
  expect_equal(obj_type_friendly(array(list(1, 2, 3), dim = 1:3)), "a list array")

  expect_equal(obj_type_friendly(int()), "an empty integer vector")
  expect_equal(obj_type_friendly(matrix(1:3)), "an integer matrix")
  expect_equal(obj_type_friendly(array(1:3, dim = 1:3)), "an integer array")

  expect_equal(obj_type_friendly(chr()), "an empty character vector")
  expect_equal(obj_type_friendly(matrix(letters)), "a character matrix")
  expect_equal(obj_type_friendly(array(letters[1:3], dim = 1:3)), "a character array")
})

test_that("obj_type_friendly() supports missing arguments", {
  expect_equal(obj_type_friendly(missing_arg()), "absent")
})

test_that("obj_type_friendly() handles scalars", {
  expect_equal(obj_type_friendly(NA), "`NA`")
  expect_equal(obj_type_friendly(na_int), "an integer `NA`")
  expect_equal(obj_type_friendly(na_dbl), "a numeric `NA`")
  expect_equal(obj_type_friendly(na_cpl), "a complex `NA`")
  expect_equal(obj_type_friendly(na_chr), "a character `NA`")

  expect_equal(obj_type_friendly(TRUE), "`TRUE`")
  expect_equal(obj_type_friendly(FALSE), "`FALSE`")

  expect_equal(obj_type_friendly(1L), "an integer")
  expect_equal(obj_type_friendly(1.0), "a number")
  expect_equal(obj_type_friendly(1i), "a complex number")
  expect_equal(obj_type_friendly(as.raw(1)), "a raw value")

  expect_equal(obj_type_friendly("foo"), "a string")
  expect_equal(obj_type_friendly(""), "`\"\"`")

  expect_equal(obj_type_friendly(list(1)), "a list")

  expect_equal(obj_type_friendly(matrix(NA)), "a logical matrix")
  expect_equal(obj_type_friendly(matrix(1)), "a double matrix")
})

test_that("obj_type_friendly() handles empty vectors", {
  expect_equal(obj_type_friendly(lgl()), "an empty logical vector")
  expect_equal(obj_type_friendly(int()), "an empty integer vector")
  expect_equal(obj_type_friendly(dbl()), "an empty numeric vector")
  expect_equal(obj_type_friendly(cpl()), "an empty complex vector")
  expect_equal(obj_type_friendly(chr()), "an empty character vector")
  expect_equal(obj_type_friendly(raw()), "an empty raw vector")
  expect_equal(obj_type_friendly(list()), "an empty list")
})
