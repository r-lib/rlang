test_that("bytes() coerces unspecified vectors but not logical ones", {
  expect_equal(bytes2(c(NA, NA), NA), new_bytes(dbl(NA, NA, NA)))
  expect_error(bytes2(TRUE), "Can't coerce")
})

test_that("can create empty and unspecified bytes() vector", {
  expect_equal(bytes2(), new_bytes(dbl()))
  expect_equal(bytes2(NA), new_bytes(na_dbl))
  expect_equal(bytes2(NA, NA), new_bytes(c(na_dbl, na_dbl)))
})

test_that("bytes() only accepts bare vectors", {
  expect_error(bytes2(factor("1Mb")), "Can't coerce")
})

test_that("as_bytes() accepts numeric input unchanged", {
  expect_equal(unclass(as_bytes(123L)), 123L)
  expect_equal(unclass(as_bytes(123)), 123)
})

test_that("as_bytes() accepts bench_byte input unchanged", {
  x <- as_bytes(123)
  expect_equal(as_bytes(x), x)
})

test_that("parse_bytes() parses character input", {
  expect_equal(unclass(parse_bytes("1")), 1)
  expect_equal(unclass(parse_bytes("1K")), 1024)
  expect_equal(unclass(parse_bytes("1M")), 1024 * 1024)
  expect_equal(unclass(parse_bytes("10M")), 10 * 1024 * 1024)
  expect_equal(unclass(parse_bytes("1G")), 1024 * 1024 * 1024)
})

test_that("format.rlib_bytes() formats bytes under 1024 as whole numbers", {
  expect_equal(format(bytes2(0)), "0B")
  expect_equal(format(bytes2(1)), "1B")
  expect_equal(format(bytes2(1023)), "1023B")
})

test_that("format.rlib_bytes() formats bytes 1024 and up as abbreviated numbers", {
  expect_equal(format(bytes2(1024)), "1KB")
  expect_equal(format(bytes2(1025)), "1KB")
  expect_equal(format(bytes2(2^16)), "64KB")
  expect_equal(format(bytes2(2^24)), "16MB")
  expect_equal(format(bytes2(2^24 + 555555)), "16.5MB")
  expect_equal(format(bytes2(2^32)), "4GB")
  expect_equal(format(bytes2(2^48)), "256TB")
  expect_equal(format(bytes2(2^64)), "16EB")
})

test_that("format.rlib_bytes() handles NA and NaN", {
  expect_equal(format(bytes2(NA)), "NA")
  expect_equal(format(bytes2(NaN)), "NaN")
})

test_that("format.rlib_bytes() works with vectors", {
  expect_equal(
    format(as_bytes(c(NA, 1, 2^13, 2^20, NaN, 2^15)), trim = TRUE),
    c("NA", "1B", "8KB", "1MB", "NaN", "32KB")
  )
  expect_equal(
    format(as_bytes(numeric())),
    character()
  )
})

test_that("sum.rlib_bytes() sums its input and returns a bench_byte", {
  expect_equal(sum(bytes2(0)), new_bytes(0))
  expect_equal(sum(bytes2(c(1, 2))), new_bytes(3))
  expect_equal(sum(bytes2(c(1, NA))), new_bytes(NA_real_))
})

test_that("min.rlib_bytes() finds minimum input and returns a bench_byte", {
  expect_equal(min(bytes2(0)), new_bytes(0))
  expect_equal(min(bytes2(c(1, 2))), new_bytes(1))
  expect_equal(min(bytes2(c(1, NA))), new_bytes(NA_real_))
})

test_that("max.rlib_bytes() finds maximum input and returns a bench_byte", {
  expect_equal(max(bytes2(0)), new_bytes(0))
  expect_equal(max(bytes2(c(1, 2))), new_bytes(2))
  expect_equal(max(bytes2(c(1, NA))), new_bytes(NA_real_))
})

test_that("[.rlib_bytes() retains the bytes2 class", {
  x <- bytes2(c(100, 200, 300))
  expect_equal(x[], x)
  expect_equal(x[1], new_bytes(100))
  expect_equal(x[1:2], new_bytes(c(100, 200)))
})

test_that("Ops.rlib_bytes() errors for unary operators", {
  x <- bytes2(c(100, 200, 300))
  expect_error(!x, "unary `!` not defined for <rlib_bytes> objects")
  expect_error(+x, "unary `\\+` not defined for <rlib_bytes> objects")
  expect_error(-x, "unary `-` not defined for <rlib_bytes> objects")
})

test_that("Ops.rlib_bytes() works with boolean comparison operators", {
  x <- bytes2(c(100, 200, 300))
  expect_equal(x == 100, c(TRUE, FALSE, FALSE))
  expect_equal(x != 100, c(FALSE, TRUE, TRUE))
  expect_equal(x > 100, c(FALSE, TRUE, TRUE))
  expect_equal(x >= 100, c(TRUE, TRUE, TRUE))
  expect_equal(x < 200, c(TRUE, FALSE, FALSE))
  expect_equal(x <= 200, c(TRUE, TRUE, FALSE))
  expect_true(bytes2("1Mb") > "1Kb")
})

test_that("Ops.rlib_bytes() works with arithmetic operators", {
  x <- bytes2(c(100, 200, 300))
  expect_equal(x + 100, bytes2(c(200, 300, 400)))
  expect_equal(x - 100, bytes2(c(0, 100, 200)))
  expect_equal(x * 100, bytes2(c(10000, 20000, 30000)))
  expect_equal(x / 2, bytes2(c(50, 100, 150)))
  expect_equal(x ^ 2, bytes2(c(10000, 40000, 90000)))
  expect_equal(bytes2("1Mb") + "1024Kb", bytes2("2Mb"))
})

test_that("Ops.rlib_bytes() errors for other binary operators", {
  x <- bytes2(c(100, 200, 300))
  expect_error(x %% 2, "`%%` not defined for <rlib_bytes> objects")
  expect_error(x %/% 2, "`%/%` not defined for <rlib_bytes> objects")
  expect_error(x & TRUE, "`&` not defined for <rlib_bytes> objects")
  expect_error(x | TRUE, "`|` not defined for <rlib_bytes> objects")
})

test_that("print method disambiguates edge cases", {
  expect_snapshot(print(bytes2()))
  expect_snapshot(print(bytes2(NA, NA)))
})
