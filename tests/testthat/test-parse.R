context("parse")

test_that("parse_quo() etc return quosures", {
  expect_identical(parse_quo("foo(bar)", "base"), set_env(quo(foo(bar)), base_env()))
  expect_identical(parse_quos("foo(bar)\n mtcars", "base"), list(set_env(quo(foo(bar)), base_env()), set_env(quo(mtcars), base_env())))
})

test_that("parse_expr() requires scalar character", {
  expect_error(parse_expr(letters), "`x` must be a string or a R connection")
})

test_that("parse_quosure() and parse_quosures() are deprecated", {
  with_verbose_retirement({
    expect_warning(parse_quosure("foo"), "soft-deprecated")
    expect_warning(parse_quosures("foo; bar"), "soft-deprecated")
  })
})

test_that("temporary connections are closed", {
  path <- tempfile("file")
  cat("1; 2; mtcars", file = path)
  conn <- file(path)

  parse_exprs(conn)
  expect_error(summary(conn), "invalid connection")
})

test_that("parse_expr() throws meaningful error messages", {
  expect_error(parse_expr(""), "No expression to parse")
  expect_error(parse_expr("foo; bar"), "More than one expression parsed")
})
