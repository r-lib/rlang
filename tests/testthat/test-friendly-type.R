test_that("friendly_type_of() supports objects", {
  expect_equal(friendly_type_of(mtcars), "a <data.frame> object")
  expect_equal(friendly_type_of(quo(1)), "a <quosure> object")
})

test_that("friendly_type_of() supports matrices and arrays (#141)", {
  expect_equal(friendly_type_of(list()), "a list")
  expect_equal(friendly_type_of(matrix(list(1, 2))), "a list matrix")
  expect_equal(friendly_type_of(array(list(1, 2, 3), dim = 1:3)), "a list array")

  expect_equal(friendly_type_of(int()), "an integer vector")
  expect_equal(friendly_type_of(matrix(1:3)), "an integer matrix")
  expect_equal(friendly_type_of(array(1:3, dim = 1:3)), "an integer array")

  expect_equal(friendly_type_of(chr()), "a character vector")
  expect_equal(friendly_type_of(matrix(letters)), "a character matrix")
  expect_equal(friendly_type_of(array(letters[1:3], dim = 1:3)), "a character array")
})

test_that("friendly_type_of() supports missing arguments", {
  expect_equal(friendly_type_of(missing_arg()), "absent")
})
