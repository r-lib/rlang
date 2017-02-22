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
  bar <- chr(list("cafe", c(0x63, 0x61, 0x66, 0xE9)), "latin1")
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

test_that("UTF-8 string roundtrips to symbol with intact encoding", {
  skip_on_cran() # portability
  skip_on_travis() # SJIS locale non-available

  old_locale <- suppressMessages(set_mbcs_locale())

  sjis <- string(c(0x90, 0xac, 0x8c, 0xf0, 0x93, 0xfa, 0x8a, 0xfa))
  utf8 <- iconv(sjis, from = "SJIS", to = "UTF-8")
  roundtrip <- function(x, f) as.character(f(x))

  # SJIS roundtrip:
  expect_identical(as_bytes(roundtrip(sjis, symbol)), as_bytes(sjis))
  expect_identical(str_encoding(roundtrip(sjis, symbol)), "unknown")

  # UTF-8 string has correct tag after roundtrip:
  expect_identical(as_bytes(roundtrip(utf8, symbol)), as_bytes(utf8))
  expect_identical(str_encoding(roundtrip(utf8, symbol)), "UTF-8")

  # And now as.name() works too because of symbol caching:
  expect_identical(str_encoding(roundtrip(utf8, as.name)), "UTF-8")


  # Let's create a new uncached string:
  sjis <- string(c(0x90, 0xac, 0x8c, 0xf0, 0x93, 0xfa, 0x8a, 0xfa, 0x8a, 0xfa))
  utf8 <- iconv(sjis, from = "SJIS", to = "UTF-8")

  # as.name() will create a symbol with untagged CHARSXP:
  expect_identical(as_bytes(roundtrip(utf8, as.name)), as_bytes(utf8))
  expect_identical(str_encoding(roundtrip(utf8, as.name)), "unknown")

  # And now symbol() fails again because the bad symbol is cached:
  expect_identical(str_encoding(roundtrip(utf8, symbol)), "unknown")

  Sys.setlocale("LC_CTYPE", old_locale)
})
