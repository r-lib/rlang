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
  expect_identical(chr_bytes(bar), bytes)
  expect_identical(str_encoding(bar[[2]]), "latin1")
})
