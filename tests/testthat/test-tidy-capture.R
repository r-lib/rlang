context("tidy capture")

test_that("explicit dots make a list of formulas", {
  fs <- quos(x = 1 + 2, y = 2 + 3)
  f1 <- as_quosure(~ 1 + 2)
  f2 <- as_quosure(~ 2 + 3)

  expect_identical(fs$x, f1)
  expect_identical(fs$y, f2)
})

test_that("quos() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = quos(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(out$dots$x, set_env(quo(x), out$env))
  expect_identical(out$dots$y, set_env(quo(a + b), out$env))
  expect_identical(out$dots$z, quo(a + b))
})

test_that("dots are interpolated", {
  fn <- function(...) {
    baz <- "baz"
    fn_var <- quo(baz)
    g(..., toupper(!! fn_var))
  }
  g <- function(...) {
    foo <- "foo"
    g_var <- quo(foo)
    h(toupper(!! g_var), ...)
  }
  h <- function(...) {
    quos(...)
  }

  bar <- "bar"
  var <- quo(bar)
  dots <- fn(toupper(!!var))

  expect_identical(map(dots, deparse), named_list("~toupper(~foo)", "~toupper(~bar)", "~toupper(~baz)"))
  expect_identical(map(dots, eval_tidy), named_list("FOO", "BAR", "BAZ"))
})

test_that("dots capture is stack-consistent", {
  fn <- function(...) {
    g(quos(...))
  }
  g <- function(dots) {
    h(dots, foo(bar))
  }
  h <- function(dots, ...) {
    dots
  }
  expect_identical(fn(foo(baz)), quos_list(quo(foo(baz))))
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
      out = g(!!! quos(...), bar(baz), !!! list(a = var, b = ~foo)),
      env = get_env()
    )
  }
  g <- function(...) {
    quos(...)
  }

  out <- fn(foo(bar))
  expected <- quos_list(
    quo(foo(bar)),
    set_env(quo(bar(baz)), out$env),
    a = quo("var"),
    b = set_env(quo(!! with_env(out$env, ~foo)), out$env)
  )
  expect_identical(out$out, expected)
})

test_that("spliced dots are wrapped in formulas", {
  args <- alist(x = var, y = foo(bar))
  expect_identical(quos(!!! args), quos_list(x = quo(var), y = quo(foo(bar))))
})

test_that("dot names are interpolated", {
  var <- "baz"
  expect_identical(quos(!!var := foo, !!toupper(var) := bar), quos_list(baz = quo(foo), BAZ = quo(bar)))
  expect_identical(quos(!!var := foo, bar), quos_list(baz = quo(foo), quo(bar)))

  var <- quote(baz)
  expect_identical(quos(!!var := foo), quos_list(baz = quo(foo)))
})

test_that("corner cases are handled when interpolating dot names", {
    var <- na_chr
    expect_identical(names(quos(!!var := NULL)), na_chr)

    var <- NULL
    expect_error(quos(!!var := NULL), "must be a name or string")
})

test_that("definitions are interpolated", {
  var1 <- "foo"
  var2 <- "bar"
  dots <- dots_definitions(def = foo(!!var1) := bar(!!var2))

  pat <- list(lhs = quo(foo("foo")), rhs = quo(bar("bar")))
  expect_identical(dots$defs$def, pat)
})

test_that("dots are forwarded to named arguments", {
  outer <- function(...) inner(...)
  inner <- function(...) fn(...)
  fn <- function(x) enquo(x)

  env <- child_env(get_env())
  expect_identical(with_env(env, outer(foo(bar))), new_quosure(quote(foo(bar)), env))
})

test_that("pronouns are scoped throughout nested captures", {
  outer <- function(data, ...) eval_tidy(quos(...)[[1]], data = data)
  inner <- function(...) map(quos(...), eval_tidy)

  data <- list(foo = "bar", baz = "baz")
  baz <- "bazz"

  expect_identical(outer(data, inner(foo, baz)), set_names(list("bar", "baz"), c("", "")))
})

test_that("Can supply := with LHS even if .named = TRUE", {
  expect_warning(regexp = NA, expect_identical(
    quos(!!"nm" := 2, .named = TRUE), quos_list(nm = as_quosure(quote(2), empty_env()))
  ))
  expect_warning(regexp = "name ignored", expect_identical(
    quos(foobar = !!"nm" := 2, .named = TRUE), quos_list(nm = as_quosure(quote(2), empty_env()))
  ))
})

test_that("RHS of tidy defs are unquoted", {
  expect_identical(quos(foo := !!"bar"), quos_list(foo = as_quosure(quote("bar"), empty_env())))
})

test_that("can capture empty list of dots", {
  fn <- function(...) quos(...)
  expect_identical(fn(), quos_list())
})

test_that("quosures are spliced before serialisation", {
  quosures <- quos(!! quo(foo(!! quo(bar))), .named = TRUE)
  expect_identical(names(quosures), "foo(bar)")
})

test_that("missing arguments are captured", {
  q <- quo()
  expect_true(is_missing(f_rhs(q)))
  expect_identical(f_env(q), empty_env())
})

test_that("empty quosures are forwarded", {
  inner <- function(x) enquo(x)
  outer <- function(x) inner(x)
  expect_identical(outer(), quo())

  inner <- function(x) enquo(x)
  outer <- function(x) inner(!! enquo(x))
  expect_identical(outer(), quo())
})

test_that("quos() captures missing arguments", {
  expect_identical(quos(, , .ignore_empty = "none"), quos_list(quo(), quo()), c("", ""))
})

test_that("quos() ignores missing arguments", {
  expect_identical(quos(, , "foo", ), quos_list(quo(), quo(), new_quosure("foo", empty_env())))
  expect_identical(quos(, , "foo", , .ignore_empty = "all"), quos_list(new_quosure("foo", empty_env())))
})

test_that("quosured literals are forwarded as is", {
  expect_identical(quo(!! quo(NULL)), new_quosure(NULL, empty_env()))
  expect_identical(quos(!! quo(10L)), set_names(quos_list(new_quosure(10L, empty_env())), ""))
})

test_that("expr() returns missing argument", {
  expect_true(is_missing(expr()))
})

test_that("expr() supports forwarded arguments", {
  fn <- function(...) g(...)
  g <- function(...) expr(...)
  expect_identical(fn(foo), quote(foo))
})

test_that("can take forced promise with strict = FALSE", {
  fn <- function(strict, x) {
    force(x)
    captureArg(x, strict = strict)
  }
  expect_error(fn(TRUE, letters), "already been evaluated")
  expect_identical(fn(FALSE, letters), NULL)
})
