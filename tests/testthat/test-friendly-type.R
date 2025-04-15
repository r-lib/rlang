test_that("obj_type_friendly() supports objects", {
  expect_equal(
    friendly_types(mtcars),
    c(
      object = "a <data.frame> object",
      object_no_value = "a <data.frame> object",
      vector = "a data frame",
      vector_length = "a data frame"
    )
  )
  expect_true(all(
    friendly_types(quo(1), vector = FALSE) == "a <quosure> object"
  ))
})

test_that("obj_type_friendly() only displays the first class of objects", {
  x <- structure(1, class = c("subclass", "class"))
  expect_identical(obj_type_friendly(x), "a <subclass> object")
})

test_that("obj_type_friendly() supports matrices and arrays (#141)", {
  expect_true(all(friendly_types(matrix(list(1, 2))) == "a list matrix"))
  expect_true(all(
    friendly_types(array(list(1, 2, 3), dim = 1:3)) == "a list array"
  ))

  expect_true(all(friendly_types(matrix(1:3)) == "an integer matrix"))
  expect_true(all(friendly_types(array(1:3, dim = 1:3)) == "an integer array"))

  expect_true(all(friendly_types(matrix(letters)) == "a character matrix"))
  expect_true(all(
    friendly_types(array(letters[1:3], dim = 1:3)) == "a character array"
  ))
})

test_that("obj_type_friendly() supports missing arguments", {
  expect_equal(obj_type_friendly(missing_arg()), "absent")
})

test_that("obj_type_friendly() handles scalars", {
  expect_equal(
    friendly_types(NA),
    c(
      object = "`NA`",
      object_no_value = "`NA`",
      vector = "a logical vector",
      vector_length = "a logical vector of length 1"
    )
  )
  expect_equal(
    friendly_types(na_int),
    c(
      object = "an integer `NA`",
      object_no_value = "an integer `NA`",
      vector = "an integer vector",
      vector_length = "an integer vector of length 1"
    )
  )
  expect_equal(
    friendly_types(na_dbl),
    c(
      object = "a numeric `NA`",
      object_no_value = "a numeric `NA`",
      vector = "a double vector",
      vector_length = "a double vector of length 1"
    )
  )
  expect_equal(
    friendly_types(na_cpl),
    c(
      object = "a complex `NA`",
      object_no_value = "a complex `NA`",
      vector = "a complex vector",
      vector_length = "a complex vector of length 1"
    )
  )
  expect_equal(
    friendly_types(na_chr),
    c(
      object = "a character `NA`",
      object_no_value = "a character `NA`",
      vector = "a character vector",
      vector_length = "a character vector of length 1"
    )
  )

  expect_equal(
    friendly_types(TRUE),
    c(
      object = "`TRUE`",
      object_no_value = "a logical value",
      vector = "a logical vector",
      vector_length = "a logical vector of length 1"
    )
  )
  expect_equal(
    friendly_types(FALSE),
    c(
      object = "`FALSE`",
      object_no_value = "a logical value",
      vector = "a logical vector",
      vector_length = "a logical vector of length 1"
    )
  )

  expect_equal(
    friendly_types(1L),
    c(
      object = "the number 1",
      object_no_value = "an integer",
      vector = "an integer vector",
      vector_length = "an integer vector of length 1"
    )
  )
  expect_equal(
    friendly_types(1.0),
    c(
      object = "the number 1",
      object_no_value = "a number",
      vector = "a double vector",
      vector_length = "a double vector of length 1"
    )
  )
  expect_equal(
    friendly_types(1i),
    c(
      object = "the complex number 0+1i",
      object_no_value = "a complex number",
      vector = "a complex vector",
      vector_length = "a complex vector of length 1"
    )
  )
  expect_equal(
    friendly_types(as.raw(1)),
    c(
      object = "the raw value 01",
      object_no_value = "a raw value",
      vector = "a raw vector",
      vector_length = "a raw vector of length 1"
    )
  )

  expect_equal(
    friendly_types("foo"),
    c(
      object = "the string \"foo\"",
      object_no_value = "a string",
      vector = "a character vector",
      vector_length = "a character vector of length 1"
    )
  )
  expect_equal(
    friendly_types(""),
    c(
      object = "the empty string \"\"",
      object_no_value = "\"\"",
      vector = "a character vector",
      vector_length = "a character vector of length 1"
    )
  )

  expect_equal(
    friendly_types(list(1)),
    c(
      object = "a list",
      object_no_value = "a list",
      vector = "a list",
      vector_length = "a list of length 1"
    )
  )
  expect_setequal(friendly_types(matrix(NA)), "a logical matrix")
  expect_setequal(friendly_types(matrix(1)), "a double matrix")
})

test_that("obj_type_friendly() handles empty vectors", {
  expect_equal(
    friendly_types(lgl()),
    c(
      object = "an empty logical vector",
      object_no_value = "an empty logical vector",
      vector = "a logical vector",
      vector_length = "a logical vector of length 0"
    )
  )
  expect_equal(
    friendly_types(int()),
    c(
      object = "an empty integer vector",
      object_no_value = "an empty integer vector",
      vector = "an integer vector",
      vector_length = "an integer vector of length 0"
    )
  )
  expect_equal(
    friendly_types(dbl()),
    c(
      object = "an empty numeric vector",
      object_no_value = "an empty numeric vector",
      vector = "a double vector",
      vector_length = "a double vector of length 0"
    )
  )
  expect_equal(
    friendly_types(cpl()),
    c(
      object = "an empty complex vector",
      object_no_value = "an empty complex vector",
      vector = "a complex vector",
      vector_length = "a complex vector of length 0"
    )
  )
  expect_equal(
    friendly_types(chr()),
    c(
      object = "an empty character vector",
      object_no_value = "an empty character vector",
      vector = "a character vector",
      vector_length = "a character vector of length 0"
    )
  )
  expect_equal(
    friendly_types(raw()),
    c(
      object = "an empty raw vector",
      object_no_value = "an empty raw vector",
      vector = "a raw vector",
      vector_length = "a raw vector of length 0"
    )
  )
  expect_equal(
    friendly_types(list()),
    c(
      object = "an empty list",
      object_no_value = "an empty list",
      vector = "a list",
      vector_length = "a list of length 0"
    )
  )
})

test_that("obj_type_friendly() handles NULL", {
  expect_true(all(friendly_types(NULL, vector = FALSE) == "`NULL`"))
  expect_snapshot(error = TRUE, cnd_class = TRUE, friendly_types(NULL))
})

test_that("obj_type_friendly() handles NaN and infinities", {
  expect_equal(
    friendly_types(NaN),
    c(
      object = "`NaN`",
      object_no_value = "`NaN`",
      vector = "a double vector",
      vector_length = "a double vector of length 1"
    )
  )
  expect_equal(
    friendly_types(Inf),
    c(
      object = "`Inf`",
      object_no_value = "`Inf`",
      vector = "a double vector",
      vector_length = "a double vector of length 1"
    )
  )
  expect_equal(
    friendly_types(-Inf),
    c(
      object = "`-Inf`",
      object_no_value = "`-Inf`",
      vector = "a double vector",
      vector_length = "a double vector of length 1"
    )
  )
  expect_equal(
    friendly_types(Inf + 0i),
    c(
      object = "the complex number Inf+0i",
      object_no_value = "a complex number",
      vector = "a complex vector",
      vector_length = "a complex vector of length 1"
    )
  )
})

test_that("long strings are truncated", {
  expect_equal(
    obj_type_friendly(strrep("abc", 12)),
    "the string \"abcabcabcabcabcabcabcabcabc...\""
  )
})
