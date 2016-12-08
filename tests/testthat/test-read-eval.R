context("read")

test_that("f_read() etc return correct formulas", {
  expect_identical(f_read("foo(bar)", "base"), with_env("base", ~foo(bar)))
  expect_identical(f_read_list("foo(bar)\n mtcars", "base"), with_env("base", list(~foo(bar), ~mtcars)))
})

test_that("read() requires scalar character", {
  expect_error(read(letters), "length > 1")
})
