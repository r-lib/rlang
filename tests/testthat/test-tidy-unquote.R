context("unquote")

test_that("interpolation does not recurse over spliced arguments", {
  var2 <- quote({foo; !! stop(); bar})
  expr_var2 <- tryCatch(expr(list(!!! var2)), error = identity)
  expect_false(inherits(expr_var2, "error"))
})

test_that("formulas containing unquote operators are interpolated", {
  var1 <- quo(foo)
  var2 <- local({ foo <- "baz"; quo(foo) })

  f <- expr_interp(~list(!!var1, !!var2))
  expect_identical(f, new_quosure(lang("list", as_quosure(var1), as_quosure(var2))))
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

test_that("formulas are not treated as quosures", {
  expect_identical(expr(a ~ b), quote(a ~ b))
  expect_identical(expr(~b), quote(~b))
  expect_identical(expr(!!~b), ~b)
})

test_that("unquote operators are always in scope", {
  env <- child_env("base", foo = "bar")
  f <- with_env(env, ~UQ(foo))
  expect_identical(expr_interp(f), new_quosure("bar", env))
})

test_that("can interpolate in specific env", {
  foo <- "bar"
  env <- child_env(NULL, foo = "foo")

  expanded <- expr_interp(~UQ(foo))
  expect_identical(expanded, set_env(quo("bar")))

  expanded <- expr_interp(~UQ(foo), env)
  expect_identical(expanded, set_env(quo("foo")))
})

test_that("can qualify operators with namespace", {
  # Should remove prefix only if rlang-qualified:
  expect_identical(quo(rlang::UQ(toupper("a"))), new_quosure("A", empty_env()))
  expect_identical(quo(list(rlang::UQS(list(a = 1, b = 2)))), quo(list(a = 1, b = 2)))

  # Should keep prefix otherwise:
  expect_identical(quo(other::UQ(toupper("a"))), quo(other::"A"))
  expect_identical(quo(x$UQ(toupper("a"))), quo(x$"A"))
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


# !! ----------------------------------------------------------------------

test_that("`!!` binds tightly", {
  foo <- "foo"
  expect_identical(expr(!! foo == a), quote("foo" == a))
  expect_identical(expr(!! foo * a != b), quote("foo" * a != b))
  expect_identical(expr(!! foo * a / b > c), quote("foo" * a / b > c))

  expect_identical(expr(a <= !! foo), quote(a <= "foo"))
  expect_identical(expr(a >= !! foo : b), quote(a >= "foo" : b))
  expect_identical(expr(a > !! foo * b : c), quote(a > "foo" * b : c))

  expect_identical(expr(!! foo^a^b), quote("foo"^a^b))
  expect_identical(expr(!! foo^a^b + c), quote("foo"^a^b + c))
})

test_that("`!!` handles binary and unary `-` and `+`", {
  foo <- "foo"
  expect_identical(expr(!! foo + a), quote("foo" + a))
  expect_identical(expr(!! foo - a), quote("foo" - a))

  foo <- 1L
  expect_identical(expr(!! +foo + a), quote(1L + a))
  expect_identical(expr(!! -foo - a), expr(UQ(-1L) - a))
})

test_that("`!!` handles special operators", {
  foo <- "foo"
  expect_identical(expr(!! foo %>% a), quote("foo" %>% a))
})


# UQ ----------------------------------------------------------------------

test_that("evaluates contents of UQ()", {
  expect_equal(quo(UQ(1 + 2)), ~ 3)
})

test_that("quosures are not rewrapped", {
  var <- quo(!! quo(letters))
  expect_identical(quo(!!var), quo(letters))

  var <- new_quosure(local(~letters), env = child_env(get_env()))
  expect_identical(quo(!!var), var)
})

test_that("UQ() fails if called without argument", {
  quo <- quo(UQ(NULL))
  expect_equal(quo, ~NULL)

  quo <- quo(rlang::UQ(NULL))
  expect_equal(quo, ~NULL)

  quo <- tryCatch(quo(UQ()), error = identity)
  expect_is(quo, "error")
  expect_match(quo$message, "must be called with an argument")

  quo <- tryCatch(quo(rlang::UQ()), error = identity)
  expect_is(quo, "error")
  expect_match(quo$message, "must be called with an argument")
})


# UQS ---------------------------------------------------------------------

test_that("UQS() treats atomic objects as scalar vectors", {
  expect_identical(quo(1 + !!! get_env()), quo(1 + !! get_env()))
  expect_identical(expr(c(!!! expression(1, 2))), expr(c(!! expression(1, 2))))
})

test_that("values of UQS() spliced into expression", {
  f <- quo(f(a, UQS(list(quote(b), quote(c))), d))
  expect_identical(f, quo(f(a, b, c, d)))
})

test_that("names within UQS() are preseved", {
  f <- quo(f(UQS(list(a = quote(b)))))
  expect_identical(f, quo(f(a = b)))
})

test_that("UQS() handles `{` calls", {
  expect_identical(quo(list(UQS(quote({ foo })))), quo(list(foo)))
})

test_that("splicing an empty vector works", {
  expect_identical(expr_interp(~list(!!! list())), quo(list()))
  expect_identical(expr_interp(~list(!!! character(0))), quo(list()))
  expect_identical(expr_interp(~list(!!! NULL)), quo(list()))
})

test_that("serialised unicode in argument names is unserialised on splice", {
  skip("failing")
  nms <- with_latin1_locale({
    exprs <- exprs("\u5e78" := 10)
    quos <- quos(!!! exprs)
    names(quos)
  })
  expect_identical(as_bytes(nms), as_bytes("\u5e78"))
  expect_true(all(chr_encoding(nms) == "UTF-8"))
})

test_that("can't splice at top level", {
  expect_error(expr(!!! letters), "top level")
})

test_that("can splice function body even if not a `{` block", {
  fn <- function(x) { x }
  expect_identical(exprs(!!! body(fn)), named_list(quote(x)))

  fn <- function(x) x
  expect_identical(exprs(!!! body(fn)), named_list(quote(x)))
})

test_that("splicing a pairlist has no side effect", {
  x <- pairlist(NULL)
  expr(foo(!!! x, y))
  expect_identical(x, pairlist(NULL))
})


# UQE ----------------------------------------------------------------

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
  var <- local(quo(foo))
  expect_identical(quo(!! var), as_quosure(var))
  expect_identical(quo(!! quo(foo)), quo(foo))
  expect_identical(quo(list(!!! letters[1:3])), quo(list("a", "b", "c")))
})

test_that("`!!` works in prefixed calls", {
  var <- ~cyl
  expect_identical(expr_interp(~mtcars$`!!`(var)), quo(mtcars$cyl))
  expect_identical(expr_interp(~foo$`!!`(quote(bar))), quo(foo$bar))
  expect_identical(expr_interp(~base::`!!`(~list)()), quo(base::list()))
})


# quosures -----------------------------------------------------------

test_that("quosures are created for all informative formulas", {
  foo <- local(quo(foo))
  bar <- local(quo(bar))

  interpolated <- local(quo(list(!!foo, !!bar)))
  expected <- new_quosure(lang("list", as_quosure(foo), as_quosure(bar)), env = get_env(interpolated))
  expect_identical(interpolated, expected)

  interpolated <- quo(!!interpolated)
  expect_identical(interpolated, expected)
})


# dots_values() ------------------------------------------------------

test_that("can unquote-splice symbols", {
  spliced <- ll(!!! list(quote(`_symbol`)))
  expect_identical(spliced, list(quote(`_symbol`)))
})

test_that("can unquote symbols", {
  expect_error(dots_values(!! quote(.)), "`!!` in a non-quoting function")
  expect_error(dots_values(rlang::UQ(quote(.))), "`!!` in a non-quoting function")
})
