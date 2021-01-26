test_that("simple hashes with no ALTREP and no attributes are reproducible", {
  skip_if_big_endian()
  expect_identical(hash(1), "a3f7d4a39b65b170005aafbbeed05106")
  expect_identical(hash("a"), "4d52a7da68952b85f039e85a90f9bbd2")
  expect_identical(hash(1:5 + 0L), "0d26bf75943b8e13c080c6bab12a7440")
})
