test_that("MD5 test suite hashes correctly", {
  expect_identical(hash_string(""), "d41d8cd98f00b204e9800998ecf8427e")
  expect_identical(hash_string("a"), "0cc175b9c0f1b6a831c399e269772661")
  expect_identical(hash_string("abc"), "900150983cd24fb0d6963f7d28e17f72")
  expect_identical(hash_string("message digest"), "f96b697d7cb7938d525a2f31aaf161d0")
  expect_identical(hash_string("abcdefghijklmnopqrstuvwxyz"), "c3fcd3d76192e4007dfb496cca67e13b")
  expect_identical(hash_string("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"), "d174ab98d277d9f5a5611c2c9f419d9f")
  expect_identical(hash_string("12345678901234567890123456789012345678901234567890123456789012345678901234567890"), "57edf4a22be3c955ac49da2e2107b67a")
})

test_that("only strings can be hashed", {
  expect_error(hash_string(1))
  expect_error(hash_string(c("x", "y")))
})

test_that("arbitrary R objects can be hashed", {
  expect_identical(hash(c(1, 2, 3)), "1eb8f305ff312576978586927c94382b")
  expect_identical(hash(data.frame(x = 1)), "4cd11304f85e879caf481abe9eb36816")
})
