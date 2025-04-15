test_that("multiplication works", {
  expect_identical(raw_deparse_str(raw()), "")
  expect_identical(raw_deparse_str(charToRaw("string")), "737472696e67")
  expect_identical(
    raw_deparse_str(raw(10), prefix = "'0x", suffix = "'"),
    "'0x00000000000000000000'"
  )
})
