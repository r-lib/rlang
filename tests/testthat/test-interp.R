context("interp")

test_that("interpolation does not recurse over spliced arguments", {
  var1 <- quote(!! stop())
  var2 <- quote({foo; !! stop(); bar})
  expect_error(f_quote(list(!!! var1)), NA)
  expect_error(expr_quote(list(!!! var2)), NA)
})

test_that("formulas are always inlined with expr_quote()", {
  var1 <- ~bar
  var2 <- local(~baz)
  f <- f_new(bquote(foo + .(var1) + .(var2)))
  expect_identical(f_quote(foo + UQ(var1) + UQ(var2)), f)
  expect_identical(expr_quote(foo + UQ(var1) + UQ(var2)), quote(foo + bar + baz))
})

test_that("formulas containing unquote operators are interpolated", {
  var1 <- ~foo
  var2 <- local({ foo <- "baz"; ~foo })

  f <- interp(~list(!!var1, !!var2))
  expect_identical(f, f_new(bquote(list(.(var1), .(var2)))))
})

test_that("interpolation is carried out in the right environment", {
  f <- local({ foo <- "foo"; ~!!foo })
  expect_identical(interp(f), f_new("foo", env = f_env(f)))
})

test_that("interpolation does not revisit unquoted formulas", {
  f <- ~list(!!~!!stop("should not interpolate within formulas"))
  f <- interp(f)
  expect_identical(interp(f), f)
})

test_that("two-sided formulas are not treated as fpromises", {
  expect_identical(expr_quote(a ~ b), quote(a ~ b))
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of UQ()", {
  expect_equal(f_quote(UQ(1 + 2)), ~ 3)
})

test_that("layers of unquote are not peeled off recursively upon interpolation", {
  var1 <- ~letters
  var2 <- ~!!var1
  expect_identical(f_quote(!!var2), f_new(~!!var1))

  var1 <- local(~letters)
  var2 <- local(~!!var1)
  expect_identical(interp(~!!var2), f_new(var2))
})

test_that("formulas are promised recursively during unquote", {
  var <- ~~letters
  expect_identical(f_quote(!!var), f_new(f_new(quote(~letters))))

  var <- f_new(local(~letters), env = env_new(env()))
  expect_identical(f_quote(!!var), f_new(var))
})


# UQS ---------------------------------------------------------------------

test_that("contents of UQS() must be a vector or language object", {
  expect_error(f_quote(1 + UQS(environment())), "`x` must be a vector")
})

test_that("values of UQS() spliced into expression", {
  f <- f_quote(f(a, UQS(list(quote(b), quote(c))), d))
  expect_identical(f, ~f(a, b, c, d))
})

test_that("names within UQS() are preseved", {
  f <- f_quote(f(UQS(list(a = quote(b)))))
  expect_identical(f, ~f(a = b))
})

test_that("UQS() handles language objects", {
  expect_identical(f_quote(list(UQS(quote(foo)))), ~list(foo))
  expect_identical(f_quote(list(UQS(quote({ foo })))), ~list(foo))
})


# UQF and UQE --------------------------------------------------------

test_that("UQF() guards formulas", {
  f <- local({ x <- "foo"; ~x })

  guarded <- call_new("_F", .args = f[-1])
  attributes(guarded) <- attributes(f)

  expected_f <- f_new(guarded)
  expect_identical(f_quote(UQF(f)), expected_f)
  expect_identical(f_eval(expected_f), f)
})

test_that("UQE() extracts right-hand side", {
  var <- ~cyl
  expect_identical(f_quote(mtcars$UQE(var)), ~mtcars$cyl)
  expect_identical(f_quote(mtcars$`!!`(var)), ~mtcars$cyl)
})


# bang ---------------------------------------------------------------

test_that("single ! is not treated as shortcut", {
  expect_identical(f_quote(!foo), ~!foo)
})

test_that("double and triple ! are treated as syntactic shortcuts", {
  var <- local(~foo)
  expect_identical(f_quote(!! var), f_new(var))
  expect_identical(f_quote(!! ~foo), f_new(~foo))
  expect_identical(f_quote(list(!!! letters[1:3])), ~list("a", "b", "c"))
})

test_that("`!!` works in prefixed calls", {
  var <- ~cyl
  expect_identical(interp(~mtcars$`!!`(var)), ~mtcars$cyl)
  expect_identical(interp(~foo$`!!`(quote(bar))), ~foo$bar)
  expect_identical(interp(~base::`!!`(~list)()), ~base::list())
})


# fpromises ----------------------------------------------------------

test_that("fpromises are created for all informative formulas", {
  foo <- local(~foo)
  bar <- local(~bar)

  interpolated <- local(f_quote(list(!!foo, !!bar)))
  expected <- f_new(bquote(list(.(f_rhs(f_new(foo))), .(f_rhs(f_new(bar))))), env = env(interpolated))
  expect_identical(interpolated, expected)

  interpolated <- f_quote(!!interpolated)
  expected <- f_new(expected)
  expect_identical(interpolated, expected)
})
