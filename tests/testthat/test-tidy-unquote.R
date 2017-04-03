context("unquote")

test_that("interpolation does not recurse over spliced arguments", {
  var1 <- quote(!! stop())
  var2 <- quote({foo; !! stop(); bar})
  expect_error(quo(list(!!! var1)), NA)
  expect_error(expr(list(!!! var2)), NA)
})

test_that("formulas containing unquote operators are interpolated", {
  var1 <- ~foo
  var2 <- local({ foo <- "baz"; ~foo })

  f <- expr_interp(~list(!!var1, !!var2))
  expect_identical(f, new_quosure(new_language("list", as_quosure(var1), as_quosure(var2))))
})

test_that("interpolation is carried out in the right environment", {
  f <- local({ foo <- "foo"; ~!!foo })
  expect_identical(expr_interp(f), new_quosure("foo", env = f_env(f)))
})

test_that("interpolation now revisits unquoted formulas", {
  f <- ~list(!!~!!stop("should not interpolate within formulas"))
  f <- expr_interp(f)
  # This used to be idempotent:
  expect_error(expect_false(identical(expr_interp(f), f)), "interpolate within formulas")
})

test_that("two-sided formulas are not treated as fpromises", {
  expect_identical(expr(a ~ b), quote(`_F`(a, b)))
})

test_that("unquote operators are always in scope", {
  env <- child_env("base", list(foo = "bar"))
  f <- with_env(env, ~UQ(foo))
  expect_identical(expr_interp(f), new_quosure("bar", env))
})

test_that("can interpolate in specific env", {
  foo <- "bar"
  env <- child_env(NULL, list(foo = "foo"))
  expect_identical(expr_interp(~UQ(foo)), set_env(quo("bar")))
  expect_identical(expr_interp(~UQ(foo), env), set_env(quo("foo")))
})

test_that("can qualify operators with namespace", {
  # Should remove prefix only if rlang-qualified:
  expect_identical(quo(rlang::UQ(toupper("a"))), new_quosure("A", empty_env()))
  expect_identical(quo(list(rlang::UQS(list(a = 1, b = 2)))), quo(list(a = 1, b = 2)))
  expect_identical(quo(rlang::UQF(~foo)), quo(UQF(~foo)))

  # Should keep prefix otherwise:
  expect_identical(quo(other::UQ(toupper("a"))), quo(other::"A"))
  expect_identical(quo(x$UQ(toupper("a"))), quo(x$"A"))
  expect_error(quo(list(other::UQS(list(a = 1, b = 2)))), "Cannot splice at top-level")
  expect_identical(quo(other::UQF(~foo)), quo(other::UQF(~foo)))
})

test_that("unquoting is frame-consistent", {
  defun <- quote(!! function() NULL)
  env <- child_env("base")
  expect_identical(fn_env(expr_interp(defun, env)), env)
})

test_that("unquoted quosure has S3 class", {
  quo <- quo(!! ~quo)
  expect_is(quo, "quosure")
})

test_that("unquoted quosures are not guarded", {
  quo <- eval_tidy(quo(quo(!! ~quo)))
  expect_true(is_quosure(quo))
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of UQ()", {
  expect_equal(quo(UQ(1 + 2)), ~ 3)
})

test_that("layers of unquote are not peeled off recursively upon interpolation", {
  var1 <- ~letters
  var2 <- ~!!var1
  expect_identical(quo(!!var2), as_quosure(~!!var1))

  var1 <- local(~letters)
  var2 <- local(~!!var1)
  expect_identical(expr_interp(~!!var2), new_quosure(as_quosure(var2)))
})

test_that("formulas are promised recursively during unquote", {
  var <- ~~letters
  expect_identical(quo(!!var), new_quosure(quote(~letters)))

  var <- new_quosure(local(~letters), env = child_env(get_env()))
  expect_identical(quo(!!var), var)
})

test_that("UQ() fails if called without argument", {
  expect_equal(quo(UQ(NULL)), ~NULL)
  expect_equal(quo(rlang::UQ(NULL)), ~NULL)
  expect_error(quo(UQ()), "must be called with an argument")
  expect_error(quo(rlang::UQ()), "must be called with an argument")
})


# UQS ---------------------------------------------------------------------

test_that("contents of UQS() must be a vector or language object", {
  expect_error(quo(1 + UQS(environment())), "`x` must be a vector")
})

test_that("values of UQS() spliced into expression", {
  f <- quo(f(a, UQS(list(quote(b), quote(c))), d))
  expect_identical(f, quo(f(a, b, c, d)))
})

test_that("names within UQS() are preseved", {
  f <- quo(f(UQS(list(a = quote(b)))))
  expect_identical(f, quo(f(a = b)))
})

test_that("UQS() handles language objects", {
  expect_identical(quo(list(UQS(quote(foo)))), quo(list(foo)))
  expect_identical(quo(list(UQS(quote({ foo })))), quo(list(foo)))
})

test_that("splicing an empty vector works", {
  expect_identical(expr_interp(~list(!!! list())), quo(list()))
  expect_identical(expr_interp(~list(!!! character(0))), quo(list()))
  expect_identical(expr_interp(~list(!!! NULL)), quo(list()))
})


# UQF and UQE --------------------------------------------------------

test_that("UQF() guards formulas", {
  f <- local({ x <- "foo"; ~x })

  guarded <- new_language("_F", splice(as.list(node_cdr(f))))
  attributes(guarded) <- attributes(f)

  expected_f <- new_quosure(guarded)
  expect_identical(quo(UQF(f)), expected_f)
  expect_identical(eval_tidy(expected_f), f)
})

test_that("UQE() extracts right-hand side", {
  var <- ~cyl
  expect_identical(quo(mtcars$UQE(var)), quo(mtcars$cyl))
  expect_identical(quo(mtcars$`!!`(var)), quo(mtcars$cyl))
})


# bang ---------------------------------------------------------------

test_that("single ! is not treated as shortcut", {
  expect_identical(quo(!foo), as_quosure(~!foo))
})

test_that("double and triple ! are treated as syntactic shortcuts", {
  var <- local(~foo)
  expect_identical(quo(!! var), as_quosure(var))
  expect_identical(quo(!! ~foo), quo(foo))
  expect_identical(quo(list(!!! letters[1:3])), quo(list("a", "b", "c")))
})

test_that("`!!` works in prefixed calls", {
  var <- ~cyl
  expect_identical(expr_interp(~mtcars$`!!`(var)), quo(mtcars$cyl))
  expect_identical(expr_interp(~foo$`!!`(quote(bar))), quo(foo$bar))
  expect_identical(expr_interp(~base::`!!`(~list)()), quo(base::list()))
})


# fpromises ----------------------------------------------------------

test_that("fpromises are created for all informative formulas", {
  foo <- local(~foo)
  bar <- local(~bar)

  interpolated <- local(quo(list(!!foo, !!bar)))
  expected <- new_quosure(new_language("list", as_quosure(foo), as_quosure(bar)), env = get_env(interpolated))
  expect_identical(interpolated, expected)

  interpolated <- quo(!!interpolated)
  expect_identical(interpolated, expected)
})


# dots_values() ------------------------------------------------------

test_that("can unquote-splice symbols", {
  expect_identical(dots_values(!!! list(quote(`_symbol`))), named_list(quote(`_symbol`)))
})
