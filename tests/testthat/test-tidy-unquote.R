context("unquote")

test_that("interpolation does not recurse over spliced arguments", {
  var1 <- quote(!! stop())
  var2 <- quote({foo; !! stop(); bar})
  expect_error(tidy_quote(list(!!! var1)), NA)
  expect_error(tidy_quote_expr(list(!!! var2)), NA)
})

test_that("formulas containing unquote operators are interpolated", {
  var1 <- ~foo
  var2 <- local({ foo <- "baz"; ~foo })

  f <- tidy_interp(~list(!!var1, !!var2))
  expect_identical(f, new_tidy_quote(bquote(list(.(var1), .(var2)))))
})

test_that("interpolation is carried out in the right environment", {
  f <- local({ foo <- "foo"; ~!!foo })
  expect_identical(tidy_interp(f), new_tidy_quote("foo", env = f_env(f)))
})

test_that("interpolation does not revisit unquoted formulas", {
  f <- ~list(!!~!!stop("should not interpolate within formulas"))
  f <- tidy_interp(f)
  expect_identical(tidy_interp(f), f)
})

test_that("two-sided formulas are not treated as fpromises", {
  expect_identical(tidy_quote_expr(a ~ b), quote(a ~ b))
})

test_that("unquote operators are always in scope", {
  env <- new_env("base", list(foo = "bar"))
  f <- with_env(env, ~UQ(foo))
  expect_identical(tidy_interp(f), with_env(env, ~"bar"))
})

test_that("can interpolate in specific env", {
  foo <- "bar"
  env <- new_env(NULL, list(foo = "foo"))
  expect_identical(tidy_interp(~UQ(foo)), ~"bar")
  expect_identical(tidy_interp(~UQ(foo), env), ~"foo")
})

test_that("can qualify operators with namespace", {
  # Should remove prefix only if rlang-qualified:
  expect_identical(tidy_quote(rlang::UQ(toupper("a"))), ~"A")
  expect_identical(tidy_quote(list(rlang::UQS(list(a = 1, b = 2)))), ~list(a = 1, b = 2))
  expect_identical(tidy_quote(rlang::UQF(~foo)), tidy_quote(UQF(~foo)))

  # Should keep prefix otherwise:
  expect_identical(tidy_quote(other::UQ(toupper("a"))), ~other::"A")
  expect_identical(tidy_quote(x$UQ(toupper("a"))), ~x$"A")
  expect_error(tidy_quote(list(other::UQS(list(a = 1, b = 2)))), "Cannot splice at top-level")
  expect_identical(tidy_quote(other::UQF(~foo)), tidy_quote(other::UQF(~foo)))
})

test_that("unquoting is frame-consistent", {
  defun <- quote(!! function() NULL)
  env <- new_env("base")
  expect_identical(fn_env(tidy_interp(defun, env)), env)
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of UQ()", {
  expect_equal(tidy_quote(UQ(1 + 2)), ~ 3)
})

test_that("layers of unquote are not peeled off recursively upon interpolation", {
  var1 <- ~letters
  var2 <- ~!!var1
  expect_identical(tidy_quote(!!var2), new_tidy_quote(~!!var1))

  var1 <- local(~letters)
  var2 <- local(~!!var1)
  expect_identical(tidy_interp(~!!var2), new_tidy_quote(var2))
})

test_that("formulas are promised recursively during unquote", {
  var <- ~~letters
  expect_identical(tidy_quote(!!var), new_tidy_quote(new_tidy_quote(quote(~letters))))

  var <- new_tidy_quote(local(~letters), env = new_env(env()))
  expect_identical(tidy_quote(!!var), new_tidy_quote(var))
})


# UQS ---------------------------------------------------------------------

test_that("contents of UQS() must be a vector or language object", {
  expect_error(tidy_quote(1 + UQS(environment())), "`x` must be a vector")
})

test_that("values of UQS() spliced into expression", {
  f <- tidy_quote(f(a, UQS(list(quote(b), quote(c))), d))
  expect_identical(f, ~f(a, b, c, d))
})

test_that("names within UQS() are preseved", {
  f <- tidy_quote(f(UQS(list(a = quote(b)))))
  expect_identical(f, ~f(a = b))
})

test_that("UQS() handles language objects", {
  expect_identical(tidy_quote(list(UQS(quote(foo)))), ~list(foo))
  expect_identical(tidy_quote(list(UQS(quote({ foo })))), ~list(foo))
})


# UQF and UQE --------------------------------------------------------

test_that("UQF() guards formulas", {
  f <- local({ x <- "foo"; ~x })

  guarded <- new_lang("_F", .args = f[-1])
  attributes(guarded) <- attributes(f)

  expected_f <- new_tidy_quote(guarded)
  expect_identical(tidy_quote(UQF(f)), expected_f)
  expect_identical(tidy_eval(expected_f), f)
})

test_that("UQE() extracts right-hand side", {
  var <- ~cyl
  expect_identical(tidy_quote(mtcars$UQE(var)), ~mtcars$cyl)
  expect_identical(tidy_quote(mtcars$`!!`(var)), ~mtcars$cyl)
})


# bang ---------------------------------------------------------------

test_that("single ! is not treated as shortcut", {
  expect_identical(tidy_quote(!foo), ~!foo)
})

test_that("double and triple ! are treated as syntactic shortcuts", {
  var <- local(~foo)
  expect_identical(tidy_quote(!! var), new_tidy_quote(var))
  expect_identical(tidy_quote(!! ~foo), new_tidy_quote(~foo))
  expect_identical(tidy_quote(list(!!! letters[1:3])), ~list("a", "b", "c"))
})

test_that("`!!` works in prefixed calls", {
  var <- ~cyl
  expect_identical(tidy_interp(~mtcars$`!!`(var)), ~mtcars$cyl)
  expect_identical(tidy_interp(~foo$`!!`(quote(bar))), ~foo$bar)
  expect_identical(tidy_interp(~base::`!!`(~list)()), ~base::list())
})


# fpromises ----------------------------------------------------------

test_that("fpromises are created for all informative formulas", {
  foo <- local(~foo)
  bar <- local(~bar)

  interpolated <- local(tidy_quote(list(!!foo, !!bar)))
  expected <- new_tidy_quote(bquote(list(.(f_rhs(new_tidy_quote(foo))), .(f_rhs(new_tidy_quote(bar))))), env = env(interpolated))
  expect_identical(interpolated, expected)

  interpolated <- tidy_quote(!!interpolated)
  expected <- new_tidy_quote(expected)
  expect_identical(interpolated, expected)
})
