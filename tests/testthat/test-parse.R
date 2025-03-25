test_that("parse_quo() etc return quosures", {
  expect_identical(
    parse_quo("foo(bar)", "base"),
    set_env(quo(foo(bar)), base_env())
  )
  expect_identical(
    parse_quos("foo(bar)\n mtcars", "base"),
    new_quosures(list(
      set_env(quo(foo(bar)), base_env()),
      set_env(quo(mtcars), base_env())
    ))
  )
})

test_that("temporary connections are closed", {
  path <- tempfile("file")
  cat("1; 2; mtcars", file = path)
  conn <- file(path)

  parse_exprs(conn)
  expect_error(summary(conn), "invalid connection")
})

test_that("parse_expr() throws meaningful error messages", {
  expect_snapshot({
    err(parse_expr(""))
    err(parse_expr("foo; bar"))
  })
})

test_that("parse_exprs() and parse_quos() handle character vectors", {
  exprs <- parse_exprs(c("foo; bar", "baz"))
  attributes(exprs) <- NULL # For srcrefs
  expect_identical(exprs, unname(exprs(foo, bar, baz)))

  quos <- parse_quos(c("foo; bar", "baz"), current_env())
  expect_identical(quos, quos(foo, bar, baz))
})

test_that("parse_exprs() requires connections or character vectors", {
  expect_error(
    parse_exprs(env()),
    "must be a character vector or an R connection"
  )
})

test_that("parse_exprs() and parse_quos() support empty input", {
  expect_identical(zap_srcref_attributes(parse_exprs(chr())), list())
  expect_identical(zap_srcref_attributes(parse_quos(chr(), env())), quos_list())
})

test_that("parse_exprs() supports empty expressions (#954)", {
  x <- c("1", "", "2")
  expect_equal(vec_unstructure(parse_exprs(x)), list(1, 2))
  expect_equal(vec_unstructure(parse_exprs("")), list())
})

test_that("parse_exprs() preserves names (#808)", {
  x <- c(a = "1 + 2; 3", b = "", c = "4")
  expect_identical(
    vec_unstructure(parse_exprs(x)),
    alist(a = 1 + 2, a = 3, c = 4)
  )
})

test_that("parse_expr() supports vectors of lines (#1540)", {
  lines <- c("{", "  a", "  b", "}")
  expect_equal(
    parse_expr(lines),
    quote({
      a
      b
    })
  )

  lines <- c("a", "b")
  expect_error(parse_expr(lines), "exactly 1 expression")
})
