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
  expect_identical(f, new_formula(NULL, lang("list", as_quosure(var1), as_quosure(var2))))
})

test_that("interpolation is carried out in the right environment", {
  f <- local({ foo <- "foo"; ~!!foo })
  expect_identical(expr_interp(f), new_formula(NULL, "foo", env = f_env(f)))
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
  expect_identical(expr_interp(f), new_formula(NULL, "bar", env))
})

test_that("can interpolate in specific env", {
  foo <- "bar"
  env <- child_env(NULL, foo = "foo")

  expanded <- expr_interp(~UQ(foo))
  expect_identical(expanded, ~"bar")

  expanded <- expr_interp(~UQ(foo), env)
  expect_identical(expanded, ~"foo")
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

  a <- b <- c <- 1
  expect_identical(expr(!! a^b^c), quote(1))
  expect_identical(expr(!! a^b^c + d), quote(1 + d))
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
  expect_identical(expr_interp(~list(!!! list())), ~list())
  expect_identical(expr_interp(~list(!!! character(0))), ~list())
  expect_identical(expr_interp(~list(!!! NULL)), ~list())
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

test_that("`!!!` works in prefix form", {
  expect_identical(exprs(`!!!`(1:2)), named_list(1L, 2L))
  expect_identical(expr(list(`!!!`(1:2))), quote(list(1L, 2L)))
  expect_identical(quos(`!!!`(1:2)), quos_list(quo(1L), quo(2L)))
  expect_identical(quo(list(`!!!`(1:2))), new_quosure(quote(list(1L, 2L))))
})


# UQE ----------------------------------------------------------------

test_that("UQE() extracts right-hand side", {
  var <- ~cyl
  expect_warning(expect_identical(quo(mtcars$UQE(var)), quo(mtcars$cyl)), "deprecated")
})

test_that("UQE() throws a deprecation warning", {
  expect_warning(exprs(UQE("foo")), "deprecated")
  expect_warning(quos(UQE("foo")), "deprecated")
  expect_warning(expr(UQE("foo")), "deprecated")
  expect_warning(quo(UQE("foo")), "deprecated")
})

test_that("UQE() can't be used in by-value dots", {
  expect_error(dots_list(UQE("foo")), "non-quoting function")
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
  var <- quo(cyl)
  expect_identical(expr_interp(~mtcars$`!!`(quo_expr(var))), ~mtcars$cyl)
  expect_identical(expr_interp(~foo$`!!`(quote(bar))), ~foo$bar)
  expect_identical(expr_interp(~base::`!!`(quote(list))()), ~base::list())
})

test_that("one layer of parentheses around !! is removed", {
  foo <- "foo"
  expect_identical(expr((!! foo)), "foo")
  expect_identical(expr(((!! foo))), quote(("foo")))

  expect_identical(expr((!! foo) + 1), quote("foo" + 1))
  expect_identical(expr(((!! foo)) + 1), quote(("foo") + 1))

  expect_identical(expr((!! sym(foo))(bar)), quote(foo(bar)))
  expect_identical(expr(((!! sym(foo)))(bar)), quote((foo)(bar)))

  expect_identical(exprs((!! foo), ((!! foo))), named_list("foo", quote(("foo"))))
})

test_that("parentheses are not removed if there's a tail", {
  expect_identical(expr((!! "a" + b)), quote(("a" + b)))
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


# := -----------------------------------------------------------------

test_that("`:=` unquotes its LHS as name unless `.unquote_names` is FALSE", {
  expect_identical(exprs(a := b), list(a = quote(b)))
  expect_identical(exprs(a := b, .unquote_names = FALSE), named_list(quote(a := b)))
  expect_identical(quos(a := b), quos_list(a = quo(b)))
  expect_identical(quos(a := b, .unquote_names = FALSE), quos_list(new_quosure(quote(a := b))))
  expect_identical(dots_list(a := NULL), list(a = NULL))
  expect_identical(dots_splice(a := NULL), list(a = NULL))
})

test_that("`:=` chaining is detected at dots capture", {
  expect_error(exprs(a := b := c), "chained")
  expect_error(quos(a := b := c), "chained")
  expect_error(dots_list(a := b := c), "chained")
  expect_error(dots_splice(a := b := c), "chained")
})

test_that("`:=` doesn't work at top level", {
  expect_error(dots_list(list(a := b), .unquote_names = FALSE), "within a quasiquoted argument")
  expect_error(dots_list(a := NULL, .unquote_names = FALSE), "within a quasiquoted argument")
  expect_error(dots_splice(a := NULL, .unquote_names = FALSE), "within a quasiquoted argument")
})
