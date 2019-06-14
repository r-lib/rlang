context("types")

test_that("predicates match definitions", {
  expect_true(is_character(letters, 26))
  expect_false(is_character(letters, 1))
  expect_false(is_list(letters, 26))

  expect_true(is_list(mtcars, 11))
  expect_false(is_list(mtcars, 0))
  expect_false(is_double(mtcars, 11))
})

test_that("can bypass string serialisation", {
  bar <- chr(list("cafe", string(c(0x63, 0x61, 0x66, 0xE9))))
  Encoding(bar) <- "latin1"
  bytes <- list(bytes(c(0x63, 0x61, 0x66, 0x65)), bytes(c(0x63, 0x61, 0x66, 0xE9)))
  expect_identical(map(bar, as_bytes), bytes)
  expect_identical(Encoding(bar[[2]]), "latin1")
})

test_that("types are friendly", {
  expect_identical(friendly_type("character"), "a character vector")
  expect_identical(friendly_type("integer"), "an integer vector")
  expect_identical(friendly_type("language"), "a call")
})

test_that("friendly_type_of() supports objects", {
  expect_identical(friendly_type_of(mtcars), "a `data.frame` object")
  expect_identical(friendly_type_of(quo(1)), "a `quosure/formula` object")
})

test_that("is_integerish() heeds type requirement", {
  for (n in 0:2) {
    expect_true(is_integerish(integer(n)))
    expect_true(is_integerish(double(n)))
    expect_false(is_integerish(double(n + 1) + .000001))
  }

  types <- c("logical", "complex", "character", "expression", "list", "raw")
  for (type in types) {
    expect_false(is_integerish(vector(type)))
  }
})

test_that("is_integerish() heeds length requirement", {
  for (n in 0:2) {
    expect_true(is_integerish(double(n), n = n))
    expect_false(is_integerish(double(n), n = n + 1))
  }
})

test_that("non finite double values are integerish", {
  expect_true(is_integerish(dbl(1, Inf, -Inf, NaN), finite = NULL))
  expect_true(is_integerish(dbl(1, NA)))
  expect_true(is_integerish(int(1, NA)))
})

test_that("is_finite handles numeric types", {
  expect_true(is_finite(1L))
  expect_false(is_finite(na_int))

  expect_true(is_finite(1))
  expect_false(is_finite(na_dbl))
  expect_false(is_finite(Inf))
  expect_false(is_finite(-Inf))
  expect_false(is_finite(NaN))
  expect_false(is_finite(c(1, 2, NaN)))

  # Should we upcoerce later on?
  expect_error(expect_false(is_finite(NA)), "expected a numeric vector")

  expect_true(is_finite(0i))
  expect_false(is_finite(complex(real = NA)))
  expect_false(is_finite(complex(imaginary = Inf)))
})

test_that("check finiteness", {
  expect_true(    is_double(dbl(1, 2), finite = TRUE))
  expect_true(is_integerish(dbl(1, 2), finite = TRUE))

  expect_false(    is_double(dbl(1, 2), finite = FALSE))
  expect_false(is_integerish(dbl(1, 2), finite = FALSE))

  expect_false(    is_double(dbl(1, Inf), finite = TRUE))
  expect_false(is_integerish(dbl(1, Inf), finite = TRUE))

  expect_true(    is_double(dbl(1, Inf), finite = FALSE))
  expect_true(is_integerish(dbl(1, Inf), finite = FALSE))

  expect_true(    is_double(dbl(-Inf, Inf), finite = FALSE))
  expect_true(is_integerish(dbl(-Inf, Inf), finite = FALSE))
})

test_that("scalar predicates heed type and length", {
  expect_true_false <- function(pred, pass, fail_len, fail_type) {
    expect_true(pred(pass))
    expect_false(pred(fail_len))
    expect_false(pred(fail_type))
  }

  expect_true_false(is_scalar_list, list(1), list(1, 2), logical(1))
  expect_true_false(is_scalar_atomic, logical(1), logical(2), list(1))
  expect_true_false(is_scalar_vector, list(1), list(1, 2), quote(x))
  expect_true_false(is_scalar_vector, logical(1), logical(2), function() {})
  expect_true_false(is_scalar_integer, integer(1), integer(2), double(1))
  expect_true_false(is_scalar_double, double(1), double(2), integer(1))
  expect_true_false(is_scalar_character, character(1), character(2), logical(1))
  expect_true_false(is_string, character(1), character(2), logical(1))
  expect_true_false(is_scalar_logical, logical(1), logical(2), character(1))
  expect_true_false(is_scalar_raw, raw(1), raw(2), NULL)
  expect_true_false(is_scalar_bytes, raw(1), raw(2), NULL)
})

test_that("is_integerish() supports large numbers (#578)", {
  expect_true(is_integerish(1e10))
  expect_true(is_integerish(2^52))

  expect_false(is_integerish(2^52 + 1))

  expect_false(is_integerish(2^50 - 0.1))
  expect_false(is_integerish(2^49 - 0.05))
  expect_false(is_integerish(2^40 - 0.0001))
})

test_that("is_string() matches on string", {
  expect_true(is_string("foo"))
  expect_true(is_string("foo", "foo"))
  expect_false(is_string("foo", "bar"))
  expect_false(is_string(NA, NA))

  expect_true(is_string("foo", c("foo", "bar")))
  expect_true(is_string("foo", c("bar", "foo")))
  expect_false(is_string("foo", c("bar", "baz")))
})

test_that("is_bool() checks for single `TRUE` or `FALSE`", {
  expect_true(is_bool(TRUE))
  expect_true(is_bool(FALSE))
  expect_false(is_bool(NA))
  expect_false(is_bool(c(TRUE, FALSE)))
})
