context("parse")

test_that("parse_f() etc return correct formulas", {
  expect_identical(parse_f("foo(bar)", "base"), with_env("base", ~foo(bar)))
  expect_identical(parse_fs("foo(bar)\n mtcars", "base"), with_env("base", list(~foo(bar), ~mtcars)))
})

test_that("parse() requires scalar character", {
  expect_error(parse_expr(letters), "`x` must be a string or a R connection")
})

test_that("temporary connections are closed", {
  path <- tempfile("file")
  cat("1; 2; mtcars", file = path)
  conn <- file(path)

  parse_exprs(conn)
  expect_error(summary(conn), "invalid connection")
})
