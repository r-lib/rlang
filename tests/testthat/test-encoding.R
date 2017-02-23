context("encoding")

test_that("can roundtrip symbols in non-UTF8 locale", {
  with_non_utf8_encoding(as_character(symbol(get_alien_lang_string())))
})
