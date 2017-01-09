context("interp")

test_that("protected against bad inputs", {
  f <- ~ x + 1
  attr(f, ".Environment") <- 10
  expect_error(interp(f), "must be an environment")
})

test_that("interpolation does not recurse over spliced arguments", {
  var1 <- quote(!! stop())
  var2 <- quote({foo; !! stop(); bar})
  expect_error(quote_f(list(!!! var1)), NA)
  expect_error(quote_expr(list(!!! var2)), NA)
})


# UQ ----------------------------------------------------------------------

make_P <- function(expr, env = parent.frame()) {
  call <- quote(`_P`(x))
  call[[2]] <- expr
  f_new(call, env = env)
}

test_that("evaluates contents of UQ()", {
  expect_equal(interp(~ UQ(1 + 2)), ~ 3)
})

test_that("layers of unquote are not peeled off recursively by interp()", {
  var1 <- ~letters
  var2 <- ~!!var1
  var3 <- ~!!var2
  expect_identical(interp(var3), ~!!var1)

  var1 <- local(~letters)
  var2 <- local(~!!var1)
  var3 <- local(~!!var2)
  nested_promises <- make_P(var2, env = f_env(var3))
  expect_identical(interp(var3), nested_promises)
})


# UQS ---------------------------------------------------------------------

test_that("contents of UQS() must be a vector or language object", {
  expr <- ~ 1 + UQS(environment())
  expect_error(interp(expr), "`x` must be a vector")
})

test_that("values of UQS() spliced into expression", {
  expr <- ~ f(a, UQS(list(quote(b), quote(c))), d)
  expect_identical(interp(expr), ~ f(a, b, c, d))
})

test_that("names within UQS() are preseved", {
  expr <- ~ f(UQS(list(a = quote(b))))
  expect_identical(interp(expr), ~ f(a = b))
})

test_that("UQS() handles language objects", {
  expect_identical(interp(~list(UQS(quote(foo)))), ~list(foo))
  expect_identical(interp(~list(UQS(quote({ foo })))), ~list(foo))
})


# UQF and UQE --------------------------------------------------------

test_that("UQF() unquotes and wraps in a formula guard", {
  f <- local(~x)
  expect_identical(interp(~ UQF(!!f)), f_new(bquote(`_F`(.(f)))))
})

test_that("UQE() extracts right-hand side", {
  var <- ~cyl
  expect_identical(interp(~mtcars$UQE(var)), ~mtcars$cyl)
  expect_identical(interp(~mtcars$`!!`(var)), ~mtcars$cyl)
})


# bang ---------------------------------------------------------------

test_that("single ! is not treated as shortcut", {
  expect_identical(interp(~!foo), ~!foo)
})

test_that("double and triple ! are treated as syntactic shortcuts", {
  var <- local(~foo)
  expect_identical(interp(~!! var), make_P(var))
  expect_identical(interp(~!! ~foo), ~foo)
  expect_identical(interp(~list(!!! letters[1:3])), ~list("a", "b", "c"))
})

test_that("`!!` works in prefixed calls", {
  var <- ~cyl
  expect_identical(interp(~mtcars$`!!`(var)), ~mtcars$cyl)
  expect_identical(interp(~foo$`!!`(quote(bar))), ~foo$bar)
  expect_identical(interp(~base::`!!`(~list)()), ~base::list())
})


# fpromises ----------------------------------------------------------

test_that("interp() is idempotent", {
  f <- ~!! ~foo
  once <- interp(f)
  twice <- interp(once)
  expect_identical(twice, once)
})

test_that("fpromises are created for all inner formulas", {
  interpolated <- interp(~~list(~foo, ~bar))
  expect_identical(interpolated, ~`_P`(~list(`_P`(~foo), `_P`(~bar))))
})
