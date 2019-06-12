context("parse")

test_that("parse_quo() etc return quosures", {
  expect_identical(parse_quo("foo(bar)", "base"), set_env(quo(foo(bar)), base_env()))
  expect_identical(parse_quos("foo(bar)\n mtcars", "base"), new_quosures(list(set_env(quo(foo(bar)), base_env()), set_env(quo(mtcars), base_env()))))
})

test_that("parse_quosure() and parse_quosures() are deprecated", {
  scoped_lifecycle_warnings()
  expect_warning(parse_quosure("foo"), "deprecated")
  expect_warning(parse_quosures("foo; bar"), "deprecated")
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

test_that("parse_exprs() and parse_quos() handle character vectors", {
  exprs <- parse_exprs(c("foo; bar", "baz"))
  attributes(exprs) <- NULL # For srcrefs
  expect_identical(exprs, unname(exprs(foo, bar, baz)))

  quos <- parse_quos(c("foo; bar", "baz"), current_env())
  expect_identical(quos, quos(foo, bar, baz))
})

test_that("parse_exprs() requires connections or character vectors", {
  expect_error(parse_exprs(env()), "must be a character vector or an R connection")
})

test_that("parse_exprs() and parse_quos() support empty input", {
  expect_identical(zap_srcref_attributes(parse_exprs(chr())), list())
  expect_identical(zap_srcref_attributes(parse_quos(chr(), env())), quos_list())
})
