context("unquote")

test_that("interpolation does not recurse over spliced arguments", {
  var1 <- quote(!! stop())
  var2 <- quote({foo; !! stop(); bar})
  expect_error(quosure(list(!!! var1)), NA)
  expect_error(expr(list(!!! var2)), NA)
})

test_that("formulas containing unquote operators are interpolated", {
  var1 <- ~foo
  var2 <- local({ foo <- "baz"; ~foo })

  f <- tidy_interp(~list(!!var1, !!var2))
  expect_identical(f, new_quosure(bquote(list(.(var1), .(var2)))))
})

test_that("interpolation is carried out in the right environment", {
  f <- local({ foo <- "foo"; ~!!foo })
  expect_identical(tidy_interp(f), new_quosure("foo", env = f_env(f)))
})

test_that("interpolation does not revisit unquoted formulas", {
  f <- ~list(!!~!!stop("should not interpolate within formulas"))
  f <- tidy_interp(f)
  expect_identical(tidy_interp(f), f)
})

test_that("two-sided formulas are not treated as fpromises", {
  expect_identical(expr(a ~ b), quote(a ~ b))
})

test_that("unquote operators are always in scope", {
  env <- child_env("base", list(foo = "bar"))
  f <- with_env(env, ~UQ(foo))
  expect_identical(tidy_interp(f), with_env(env, ~"bar"))
})

test_that("can interpolate in specific env", {
  foo <- "bar"
  env <- child_env(NULL, list(foo = "foo"))
  expect_identical(tidy_interp(~UQ(foo)), ~"bar")
  expect_identical(tidy_interp(~UQ(foo), env), ~"foo")
})

test_that("can qualify operators with namespace", {
  # Should remove prefix only if rlang-qualified:
  expect_identical(quosure(rlang::UQ(toupper("a"))), ~"A")
  expect_identical(quosure(list(rlang::UQS(list(a = 1, b = 2)))), ~list(a = 1, b = 2))
  expect_identical(quosure(rlang::UQF(~foo)), quosure(UQF(~foo)))

  # Should keep prefix otherwise:
  expect_identical(quosure(other::UQ(toupper("a"))), ~other::"A")
  expect_identical(quosure(x$UQ(toupper("a"))), ~x$"A")
  expect_error(quosure(list(other::UQS(list(a = 1, b = 2)))), "Cannot splice at top-level")
  expect_identical(quosure(other::UQF(~foo)), quosure(other::UQF(~foo)))
})

test_that("unquoting is frame-consistent", {
  defun <- quote(!! function() NULL)
  env <- child_env("base")
  expect_identical(fn_env(tidy_interp(defun, env)), env)
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of UQ()", {
  expect_equal(quosure(UQ(1 + 2)), ~ 3)
})

test_that("layers of unquote are not peeled off recursively upon interpolation", {
  var1 <- ~letters
  var2 <- ~!!var1
  expect_identical(quosure(!!var2), new_quosure(~!!var1))

  var1 <- local(~letters)
  var2 <- local(~!!var1)
  expect_identical(tidy_interp(~!!var2), new_quosure(var2))
})

test_that("formulas are promised recursively during unquote", {
  var <- ~~letters
  expect_identical(quosure(!!var), new_quosure(new_quosure(quote(~letters))))

  var <- new_quosure(local(~letters), env = child_env(get_env()))
  expect_identical(quosure(!!var), new_quosure(var))
})


# UQS ---------------------------------------------------------------------

test_that("contents of UQS() must be a vector or language object", {
  expect_error(quosure(1 + UQS(environment())), "`x` must be a vector")
})

test_that("values of UQS() spliced into expression", {
  f <- quosure(f(a, UQS(list(quote(b), quote(c))), d))
  expect_identical(f, ~f(a, b, c, d))
})

test_that("names within UQS() are preseved", {
  f <- quosure(f(UQS(list(a = quote(b)))))
  expect_identical(f, ~f(a = b))
})

test_that("UQS() handles language objects", {
  expect_identical(quosure(list(UQS(quote(foo)))), ~list(foo))
  expect_identical(quosure(list(UQS(quote({ foo })))), ~list(foo))
})

test_that("splicing an empty vector works", {
  expect_identical(tidy_interp(~list(!!! list())), ~list())
  expect_identical(tidy_interp(~list(!!! character(0))), ~list())
  expect_identical(tidy_interp(~list(!!! NULL)), ~list())
})


# UQF and UQE --------------------------------------------------------

test_that("UQF() guards formulas", {
  f <- local({ x <- "foo"; ~x })

  guarded <- new_language("_F", .args = f[-1])
  attributes(guarded) <- attributes(f)

  expected_f <- new_quosure(guarded)
  expect_identical(quosure(UQF(f)), expected_f)
  expect_identical(tidy_eval(expected_f), f)
})

test_that("UQE() extracts right-hand side", {
  var <- ~cyl
  expect_identical(quosure(mtcars$UQE(var)), ~mtcars$cyl)
  expect_identical(quosure(mtcars$`!!`(var)), ~mtcars$cyl)
})


# bang ---------------------------------------------------------------

test_that("single ! is not treated as shortcut", {
  expect_identical(quosure(!foo), ~!foo)
})

test_that("double and triple ! are treated as syntactic shortcuts", {
  var <- local(~foo)
  expect_identical(quosure(!! var), new_quosure(var))
  expect_identical(quosure(!! ~foo), new_quosure(~foo))
  expect_identical(quosure(list(!!! letters[1:3])), ~list("a", "b", "c"))
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

  interpolated <- local(quosure(list(!!foo, !!bar)))
  expected <- new_quosure(bquote(list(.(f_rhs(new_quosure(foo))), .(f_rhs(new_quosure(bar))))), env = get_env(interpolated))
  expect_identical(interpolated, expected)

  interpolated <- quosure(!!interpolated)
  expected <- new_quosure(expected)
  expect_identical(interpolated, expected)
})
