test_that("simple hashes with no ALTREP and no attributes are reproducible", {
  skip_if_big_endian()
  expect_identical(hash(1), "a3f7d4a39b65b170005aafbbeed05106")
  expect_identical(hash("a"), "4d52a7da68952b85f039e85a90f9bbd2")
  expect_identical(hash(1:5 + 0L), "0d26bf75943b8e13c080c6bab12a7440")
})

test_that("hash_file() errors if the file doesn't exist", {
  expect_error(hash_file("foo.ext"))
})

test_that("hash_file() has known fixed value for empty files", {
  skip_if_big_endian()

  path <- withr::local_tempfile()
  file.create(path)

  expect_identical(hash_file(path), "99aa06d3014798d86001c324468d497f")
})

test_that("hash_file() results change as more data is written to the file", {
  skip_if_big_endian()

  path <- withr::local_tempfile()
  file.create(path)

  initial <- hash_file(path)

  saveRDS(1, path)

  expect_true(hash_file(path) != initial)
})
