test_that("predicates match definitions", {
  expect_true(is_character(letters, 26))
  expect_false(is_character(letters, 1))
  expect_false(is_list(letters, 26))

  expect_true(is_list(mtcars, 11))
  expect_false(is_list(mtcars, 0))
  expect_false(is_double(mtcars, 11))

  expect_true(is_complex(cpl(1, 2), n = 2))
  expect_false(is_complex(cpl(1, 2), n = 3))
  expect_false(is_scalar_complex(cpl(1, 2)))
  expect_false(is_bare_complex(structure(cpl(1, 2), class = "foo")))
})

test_that("can bypass string serialisation", {
  bar <- chr(list("cafe", string(c(0x63, 0x61, 0x66, 0xE9))))
  Encoding(bar) <- "latin1"
  bytes <- list(bytes(c(0x63, 0x61, 0x66, 0x65)), bytes(c(0x63, 0x61, 0x66, 0xE9)))
  expect_identical(map(bar, charToRaw), bytes)
  expect_identical(Encoding(bar[[2]]), "latin1")
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
  expect_true(   is_complex(cpl(1, 2), finite = TRUE))
  expect_true(is_integerish(dbl(1, 2), finite = TRUE))

  expect_false(    is_double(dbl(1, 2), finite = FALSE))
  expect_false(   is_complex(cpl(1, 2), finite = FALSE))
  expect_false(is_integerish(dbl(1, 2), finite = FALSE))

  expect_false(    is_double(dbl(1, Inf), finite = TRUE))
  expect_false(   is_complex(cpl(1, Inf), finite = TRUE))
  expect_false(is_integerish(dbl(1, Inf), finite = TRUE))

  expect_true(    is_double(dbl(1, Inf), finite = FALSE))
  expect_true(   is_complex(cpl(1, Inf), finite = FALSE))
  expect_true(is_integerish(dbl(1, Inf), finite = FALSE))

  expect_true(    is_double(dbl(-Inf, Inf), finite = FALSE))
  expect_true(   is_complex(cpl(-Inf, Inf), finite = FALSE))
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

test_that("is_string2() matches on `empty`", {
  # Input checking
  expect_snapshot({
    (expect_error(is_string2("foo", empty = 1)))
    (expect_error(is_string2("foo", empty = NA)))
    (expect_error(is_string2("foo", "foo", empty = TRUE)))
  })

  expect_true(is_string2("foo", empty = NULL))
  expect_true(is_string2("foo", empty = FALSE))
  expect_false(is_string2("foo", empty = TRUE))

  expect_true(is_string2("", empty = NULL))
  expect_true(is_string2("", empty = TRUE))
  expect_false(is_string2("", empty = FALSE))
})

test_that("is_bool() checks for single `TRUE` or `FALSE`", {
  expect_true(is_bool(TRUE))
  expect_true(is_bool(FALSE))
  expect_false(is_bool(NA))
  expect_false(is_bool(c(TRUE, FALSE)))
})

test_that("is_character2() matches empty and missing values", {
  expect_true(is_character2("", empty = TRUE, missing = TRUE))
  expect_true(is_character2(na_chr, empty = TRUE, missing = TRUE))

  expect_false(is_character2(c("foo", ""), empty = FALSE))
  expect_true(is_character2(c("foo", ""), empty = TRUE))
  expect_true(is_character2(c("", ""), empty = TRUE))
  expect_true(is_character2(c("foo", "foo"), empty = FALSE))

  expect_false(is_character2(c("foo", NA), missing = FALSE))
  expect_true(is_character2(c("foo", NA), missing = TRUE))
  expect_true(is_character2(chr(NA, NA), missing = TRUE))
  expect_true(is_character2(c("foo", "foo"), missing = FALSE))

  expect_true(is_character2(c("foo", "foo"), empty = FALSE, missing = FALSE))
  expect_true(is_character2(c("foo", "foo"), empty = FALSE, missing = TRUE))
  expect_true(is_character2(chr(NA, NA), empty = FALSE, missing = TRUE))
  expect_true(is_character2(c("foo", "foo"), empty = TRUE, missing = FALSE))
  expect_true(is_character2(c("", ""), empty = TRUE, missing = FALSE))
})
