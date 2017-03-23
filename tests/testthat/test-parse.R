context("parse")

test_that("parse_quosure() etc return correct formulas", {
  expect_identical(parse_quosure("foo(bar)", "base"), set_env(quo(foo(bar)), base_env()))
  expect_identical(parse_quosures("foo(bar)\n mtcars", "base"), list(set_env(quo(foo(bar)), base_env()), set_env(quo(mtcars), base_env())))
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
