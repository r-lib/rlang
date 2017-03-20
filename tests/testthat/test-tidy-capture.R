context("tidy capture")

test_that("explicit dots make a list of formulas", {
  fs <- dots_quosures(x = 1 + 2, y = 2 + 3)
  f1 <- ~ 1 + 2
  f2 <- ~ 2 + 3

  expect_identical(fs$x, f1)
  expect_identical(fs$y, f2)
})

test_that("dots_quosures() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = dots_quosures(x = x, y = a + b, ...), env = environment())
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
    dots_quosures(...)
  }

  bar <- "bar"
  var <- ~bar
  dots <- fn(toupper(!!var))

  expect_identical(map(dots, deparse), named(list("~toupper(~foo)", "~toupper(~bar)", "~toupper(~baz)")))
  expect_identical(map(dots, eval_tidy), named(list("FOO", "BAR", "BAZ")))
})

test_that("dots capture is stack-consistent", {
  fn <- function(...) {
    g(dots_quosures(...))
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
      out = g(!!! dots_quosures(...), bar(baz), !!! list(a = var, b = ~foo)),
      env = get_env()
    )
  }
  g <- function(...) {
    dots_quosures(...)
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
  expect_identical(dots_quosures(!!! args), list(x = ~var, y = ~var))
})

test_that("dot names are interpolated", {
  var <- "baz"
  expect_identical(dots_quosures(!!var := foo, !!toupper(var) := bar), list(baz = ~foo, BAZ = ~bar))
  expect_identical(dots_quosures(!!var := foo, bar), list(baz = ~foo, ~bar))

  var <- quote(baz)
  expect_identical(dots_quosures(!!var := foo), list(baz = ~foo))
})

test_that("corner cases are handled when interpolating dot names", {
    var <- na_chr
    expect_identical(names(dots_quosures(!!var := NULL)), na_chr)

    var <- NULL
    expect_error(dots_quosures(!!var := NULL), "must be a name or string")
})

test_that("definitions are interpolated", {
  var1 <- "foo"
  var2 <- "bar"
  dots <- dots_definitions(def = foo(!!var1) := bar(!!var2))

  pat <- list(lhs = ~foo("foo"), rhs = ~bar("bar"))
  expect_identical(dots$defs$def, pat)
})

test_that("dots are forwarded to named arguments", {
  outer <- function(...) inner(...)
  inner <- function(...) fn(...)
  fn <- function(x) catch_quosure(x)

  env <- child_env(get_env())
  expect_identical(with_env(env, outer(foo(bar))), new_quosure(quote(foo(bar)), env))
})

test_that("pronouns are scoped throughout nested captures", {
  outer <- function(data, ...) eval_tidy(dots_quosures(...)[[1]], data = data)
  inner <- function(...) map(dots_quosures(...), eval_tidy)

  data <- list(foo = "bar", baz = "baz")
  baz <- "bazz"

  expect_identical(outer(data, inner(foo, baz)), set_names(list("bar", "baz"), c("", "")))
})

test_that("Can supply := with LHS even if .named = TRUE", {
  expect_warning(regexp = NA, expect_identical(
    dots_quosures(!!"nm" := 2, .named = TRUE), list(nm = ~2)
  ))
  expect_warning(regexp = "name ignored", expect_identical(
    dots_quosures(foobar = !!"nm" := 2, .named = TRUE), list(nm = ~2)
  ))
})

test_that("RHS of tidy defs are unquoted", {
  expect_identical(dots_quosures(foo := !!"bar"), list(foo = ~"bar"))
})

test_that("can capture empty list of dots", {
  fn <- function(...) dots_quosures(...)
  expect_identical(fn(), list())
})

test_that("quosures are spliced before serialisation", {
  quosures <- dots_quosures(!! ~foo(~bar), .named = TRUE)
  expect_identical(names(quosures), "foo(bar)")
})

test_that("dots_quosures() captures missing arguments", {
  q <- new_quosure(missing_arg(), empty_env())
  expect_identical(dots_quosures(, ), set_names(list(q, q), c("", "")))
})

test_that("formulas are guarded on capture", {
  expect_identical(
    quosure(~foo(~bar, ~~baz())),
    quosure(`_F`(foo(`_F`(bar), `_F`(`_F`(baz())))))
  )
})

test_that("formulas are not guarded if unquoted", {
  expect_identical(
    quosure(!! ~foo(~bar, ~~baz())),
    new_quosure(~foo(~bar, ~~baz()))
  )
  quo <- quosure(foo(bar))
  quo <- quosure(baz(!! quo))
  expect_equal(quo, ~baz(~foo(bar)))
})

test_that("quosured literals are forwarded as is", {
  expect_identical(quosure(!! ~NULL), ~NULL)
  expect_identical(quosure(!! quosure(NULL)), new_quosure(NULL, empty_env()))
  expect_identical(dots_quosures(!! ~10L), set_names(list(~10L), ""))
})
