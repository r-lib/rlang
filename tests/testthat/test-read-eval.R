context("parse")

test_that("f_parse_expr() etc return correct formulas", {
  expect_identical(f_parse_expr("foo(bar)", "base"), with_env("base", ~foo(bar)))
  expect_identical(f_parse_exprs("foo(bar)\n mtcars", "base"), with_env("base", list(~foo(bar), ~mtcars)))
})

test_that("parse() requires scalar character", {
  expect_error(parse_expr(letters), "`x` must be a string or a R connection")
})


context("eval")

test_that("invoke() buries arguments", {
  expect_identical(invoke(call_inspect, 1:2), quote(.fn(`1`, `2`)))
  expect_identical(invoke("call_inspect", 1:2), quote(call_inspect(`1`, `2`)))
  expect_identical(invoke(call_inspect, 1:2, .bury = c("foo", "bar")), quote(foo(`bar1`, `bar2`)))
  expect_identical(invoke(call_inspect, 1:2, .bury = FALSE), as.call(list(call_inspect, 1L, 2L)))
})

test_that("invoke() is calls without arguments", {
  expect_identical(invoke("list"), list())
  expect_identical(invoke(list), list())
})
