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
  bar <- chr(list("cafe", string(c(0x63, 0x61, 0x66, 0xE9))), .encoding = "latin1")
  bytes <- list(bytes(c(0x63, 0x61, 0x66, 0x65)), bytes(c(0x63, 0x61, 0x66, 0xE9)))
  expect_identical(map(bar, as_bytes), bytes)
  expect_identical(str_encoding(bar[[2]]), "latin1")
})

test_that("pattern match on string encoding", {
  expect_true(is_character(letters, encoding = "unknown"))
  expect_false(is_character(letters, encoding = "UTF-8"))

  chr <- chr(c("foo", "fo\uE9"))
  expect_false(is_character(chr, encoding = "UTF-8"))
  expect_false(is_character(chr, encoding = "unknown"))
  expect_true(is_character(chr, encoding = c("unknown", "UTF-8")))
})

test_that("type_of() returns correct type", {
  expect_identical(type_of("foo"), "string")
  expect_identical(type_of(letters), "character")
  expect_identical(type_of(base::`$`), "primitive")
  expect_identical(type_of(base::list), "primitive")
  expect_identical(type_of(base::eval), "closure")
  expect_identical(type_of(~foo), "formula")
  expect_identical(type_of(quo(foo)), "formula")
  expect_identical(type_of(quote(a := b)), "definition")
  expect_identical(type_of(quote(foo())), "language")
})

test_that("lang_type_of() returns correct lang subtype", {
  expect_identical(lang_type_of(quote(foo())), "named")
  expect_identical(lang_type_of(quote(foo::bar())), "namespaced")
  expect_identical(lang_type_of(quote(foo@bar())), "recursive")

  lang <- quote(foo())
  node_poke_car(lang, 10)
  expect_error(lang_type_of(lang), "corrupt")

  node_poke_car(lang, base::list)
  expect_identical(lang_type_of(lang), "inlined")
})

test_that("types are friendly", {
  expect_identical(friendly_type("character"), "a character vector")
  expect_identical(friendly_type("integer"), "an integer vector")
  expect_identical(friendly_type("language"), "a call")
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

test_that("check finiteness", {
  expect_true(    is_double(dbl(1, 2), finite = TRUE))
  expect_true(is_integerish(dbl(1, 2), finite = TRUE))

  expect_false(    is_double(dbl(1, Inf), finite = TRUE))
  expect_false(is_integerish(dbl(1, Inf), finite = TRUE))

  expect_false(    is_double(dbl(1, Inf), finite = FALSE))
  expect_false(is_integerish(dbl(1, Inf), finite = FALSE))

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
