context("tidy capture")

test_that("explicit dots make a list of formulas", {
  fs <- tidy_quotes(x = 1 + 2, y = 2 + 3)
  f1 <- ~ 1 + 2
  f2 <- ~ 2 + 3

  expect_identical(fs$x, f1)
  expect_identical(fs$y, f2)
})

test_that("tidy_quotes() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = tidy_quotes(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(out$dots$x, with_env(out$env, ~x))
  expect_identical(out$dots$y, with_env(out$env, ~a + b))
  expect_identical(out$dots$z, ~a + b)
})

test_that("dots are interpolated", {
  fn <- function(...) {
    baz <- "baz"
    fn_var <- ~baz
    g(..., toupper(!! fn_var))
  }
  g <- function(...) {
    foo <- "foo"
    g_var <- ~foo
    h(toupper(!! g_var), ...)
  }
  h <- function(...) {
    tidy_quotes(...)
  }

  bar <- "bar"
  var <- ~bar
  dots <- fn(toupper(!!var))

  expect_identical(map(dots, deparse), named(list("~toupper(~foo)", "~toupper(~bar)", "~toupper(~baz)")))
  expect_identical(map(dots, tidy_eval), named(list("FOO", "BAR", "BAZ")))
})

test_that("dots capture is stack-consistent", {
  fn <- function(...) {
    g(tidy_quotes(...))
  }
  g <- function(dots) {
    h(dots, foo(bar))
  }
  h <- function(dots, ...) {
    dots
  }
  expect_identical(fn(foo(baz)), named(list(~foo(baz))))
})

test_that("splice is consistently recognised", {
  expect_true(is_splice(quote(!!! list())))
  expect_true(is_splice(quote(UQS(list()))))
  expect_true(is_splice(quote(rlang::UQS(list()))))
  expect_false(is_splice(quote(ns::UQS(list()))))
})

test_that("dots can be spliced in", {
  fn <- function(...) {
    var <- "var"
    list(
      out = g(!!! tidy_quotes(...), bar(baz), !!! list(a = var, b = ~foo)),
      env = env()
    )
  }
  g <- function(...) {
    tidy_quotes(...)
  }

  out <- fn(foo(bar))
  expected <- list(
    ~foo(bar),
    with_env(out$env, ~bar(baz)),
    a = with_env(out$env, ~"var"),
    b = with_env(out$env, ~foo)
  )
  expect_identical(out$out, expected)
})

test_that("spliced dots are wrapped in formulas", {
  args <- alist(x = var, y = ~var)
  expect_identical(tidy_quotes(!!! args), list(x = ~var, y = ~var))
})

test_that("dot names are interpolated", {
  var <- "baz"
  expect_identical(tidy_quotes(!!var := foo, !!toupper(var) := bar), list(baz = ~foo, BAZ = ~bar))
  expect_identical(tidy_quotes(!!var := foo, bar), list(baz = ~foo, ~bar))

  var <- quote(baz)
  expect_identical(tidy_quotes(!!var := foo), list(baz = ~foo))

  def <- !!var := foo
  expect_identical(tidy_quotes(!! def), list(baz = ~foo))
})

test_that("corner cases are handled when interpolating dot names", {
    var <- na_chr
    expect_identical(names(tidy_quotes(!!var := NULL)), na_chr)

    var <- NULL
    expect_error(tidy_quotes(!!var := NULL), "must be a name or string")
})

test_that("definitions are interpolated", {
  var1 <- "foo"
  var2 <- "bar"
  dots <- tidy_defs(def = foo(!!var1) := bar(!!var2))

  pat <- list(lhs = ~foo("foo"), rhs = ~bar("bar"))
  expect_identical(dots$defs$def, pat)
})

test_that("dots are forwarded to named arguments", {
  outer <- function(...) inner(...)
  inner <- function(...) fn(...)
  fn <- function(x) tidy_capture(x)

  env <- child_env(env())
  expect_identical(with_env(env, outer(foo(bar))), new_quosure(quote(foo(bar)), env))
})

test_that("pronouns are scoped throughout nested captures", {
  outer <- function(data, ...) tidy_eval(tidy_quotes(...)[[1]], data = data)
  inner <- function(...) map(tidy_quotes(...), tidy_eval)

  data <- list(foo = "bar", baz = "baz")
  baz <- "bazz"

  expect_identical(outer(data, inner(foo, baz)), set_names(list("bar", "baz"), c("", "")))
})

test_that("Can supply := with LHS even if .named = TRUE", {
  expect_warning(regexp = NA, expect_identical(
    tidy_quotes(!!"nm" := 2, .named = TRUE), list(nm = ~2)
  ))
  expect_warning(regexp = "name ignored", expect_identical(
    tidy_quotes(foobar = !!"nm" := 2, .named = TRUE), list(nm = ~2)
  ))
})

test_that("RHS of tidy defs are unquoted", {
  expect_identical(tidy_quotes(foo := !!"bar"), list(foo = ~"bar"))
})

test_that("can capture empty list of dots", {
  fn <- function(...) tidy_quotes(...)
  expect_identical(fn(), list())
})

test_that("quosures are spliced before serialisation", {
  quosures <- tidy_quotes(~foo(~bar), .named = TRUE)
  expect_identical(names(quosures), "foo(bar)")
})

test_that("tidy_quotes() captures missing arguments", {
  q <- new_quosure(arg_missing(), empty_env())
  expect_identical(tidy_quotes(, ), set_names(list(q, q), c("", "")))
})
