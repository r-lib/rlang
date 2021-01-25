test_that("simple hashes with no ALTREP and no attributes are reproducible", {
  expect_identical(hash(1), "e102171758b6df27bab06e4b99ad7d61")
  expect_identical(hash("a"), "49a20bac2e944a3b87d0f5f70b8b5553")
  expect_identical(hash(1:5 + 0L), "f24b071216a7d95ccbfcf28616107d64")
})
