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
  expect_identical(type_of(a := b), "definition")
  expect_identical(type_of(quote(foo())), "language")
})

test_that("lang_type_of() returns correct lang subtype", {
  expect_identical(lang_type_of(quote(foo())), "named")
  expect_identical(lang_type_of(quote(foo::bar())), "namespaced")
  expect_identical(lang_type_of(quote(foo@bar())), "recursive")

  lang <- quote(foo())
  mut_node_car(lang, 10)
  expect_error(lang_type_of(lang), "corrupt")

  mut_node_car(lang, base::list)
  expect_identical(lang_type_of(lang), "inlined")
})

test_that("types are friendly", {
  expect_identical(friendly_type("character"), "a character vector")
  expect_identical(friendly_type("integer"), "an integer vector")
  expect_identical(friendly_type("language"), "a call")
})
