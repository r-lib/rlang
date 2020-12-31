test_that("simple hashes with no ALTREP and no attributes are reproducible", {
  if (.Platform$endian == "little") {
    expect_identical(hash(1), "a3f7d4a39b65b170005aafbbeed05106")
    expect_identical(hash("a"), "4d52a7da68952b85f039e85a90f9bbd2")
    expect_identical(hash(1:5 + 0L), "0d26bf75943b8e13c080c6bab12a7440")
  } else {
    expect_identical(hash(1), "e102171758b6df27bab06e4b99ad7d61")
    expect_identical(hash("a"), "49a20bac2e944a3b87d0f5f70b8b5553")
    expect_identical(hash(1:5 + 0L), "f24b071216a7d95ccbfcf28616107d64")
  }
})
