context("tidy capture")

quos_list <- function(...) {
  quos <- list(...)
  if (length(quos)) {
    names(quos) <- names2(quos)
  }
  struct(quos, class = "quosures")
}

test_that("explicit dots make a list of formulas", {
  fs <- dots_quos(x = 1 + 2, y = 2 + 3)
  f1 <- as_quosure(~ 1 + 2)
  f2 <- as_quosure(~ 2 + 3)

  expect_identical(fs$x, f1)
  expect_identical(fs$y, f2)
})

test_that("dots_quos() produces correct formulas", {
  fn <- function(x = a + b, ...) {
    list(dots = dots_quos(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(out$dots$x, set_env(quo(x), out$env))
  expect_identical(out$dots$y, set_env(quo(a + b), out$env))
  expect_identical(out$dots$z, quo(a + b))
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
    dots_quos(...)
  }

  bar <- "bar"
  var <- ~bar
  dots <- fn(toupper(!!var))

  expect_identical(map(dots, deparse), named(list("~toupper(~foo)", "~toupper(~bar)", "~toupper(~baz)")))
  expect_identical(map(dots, eval_tidy), named(list("FOO", "BAR", "BAZ")))
})

test_that("dots capture is stack-consistent", {
  fn <- function(...) {
    g(dots_quos(...))
  }
  g <- function(dots) {
    h(dots, foo(bar))
  }
  h <- function(dots, ...) {
    dots
  }
  expect_identical(fn(foo(baz)), named(quos_list(quo(foo(baz)))))
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
      out = g(!!! dots_quos(...), bar(baz), !!! list(a = var, b = ~foo)),
      env = get_env()
    )
  }
  g <- function(...) {
    dots_quos(...)
  }

  out <- fn(foo(bar))
  expected <- quos_list(
    quo(foo(bar)),
    set_env(quo(bar(baz)), out$env),
    a = set_env(quo("var"), out$env),
    b = set_env(quo(foo), out$env)
  )
  expect_identical(out$out, expected)
})

test_that("spliced dots are wrapped in formulas", {
  args <- alist(x = var, y = ~var)
  expect_identical(dots_quos(!!! args), quos_list(x = quo(var), y = quo(var)))
})

test_that("dot names are interpolated", {
  var <- "baz"
  expect_identical(dots_quos(!!var := foo, !!toupper(var) := bar), quos_list(baz = quo(foo), BAZ = quo(bar)))
  expect_identical(dots_quos(!!var := foo, bar), quos_list(baz = quo(foo), quo(bar)))

  var <- quote(baz)
  expect_identical(dots_quos(!!var := foo), quos_list(baz = quo(foo)))
})

test_that("corner cases are handled when interpolating dot names", {
    var <- na_chr
    expect_identical(names(dots_quos(!!var := NULL)), na_chr)

    var <- NULL
    expect_error(dots_quos(!!var := NULL), "must be a name or string")
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
  outer <- function(data, ...) eval_tidy(dots_quos(...)[[1]], data = data)
  inner <- function(...) map(dots_quos(...), eval_tidy)

  data <- list(foo = "bar", baz = "baz")
  baz <- "bazz"

  expect_identical(outer(data, inner(foo, baz)), set_names(list("bar", "baz"), c("", "")))
})

test_that("Can supply := with LHS even if .named = TRUE", {
  expect_warning(regexp = NA, expect_identical(
    dots_quos(!!"nm" := 2, .named = TRUE), quos_list(nm = as_quosure(quote(2)))
  ))
  expect_warning(regexp = "name ignored", expect_identical(
    dots_quos(foobar = !!"nm" := 2, .named = TRUE), quos_list(nm = as_quosure(quote(2)))
  ))
})

test_that("RHS of tidy defs are unquoted", {
  expect_identical(dots_quos(foo := !!"bar"), quos_list(foo = as_quosure(quote("bar"))))
})

test_that("can capture empty list of dots", {
  fn <- function(...) dots_quos(...)
  expect_identical(fn(), quos_list())
})

test_that("quosures are spliced before serialisation", {
  quosures <- dots_quos(!! ~foo(~bar), .named = TRUE)
  expect_identical(names(quosures), "foo(bar)")
})

test_that("dots_quos() captures missing arguments", {
  q <- new_quosure(missing_arg(), empty_env())
  expect_identical(dots_quos(, ), quos_list(q, q))
})

test_that("formulas are guarded on capture", {
  expect_identical(
    quo(~foo(~bar, ~~baz())),
    quo(`_F`(foo(`_F`(bar), `_F`(`_F`(baz())))))
  )
})

test_that("formulas are not guarded if unquoted", {
  expect_identical(
    quo(!! ~foo(~bar, ~~baz())),
    new_quosure(new_quosure(quote(foo(~bar, ~~baz()))))
  )
  quo <- quo(foo(bar))
  quo <- quo(baz(!! quo))
  expect_equal(quo, ~baz(~foo(bar)))
})

test_that("quosured literals are forwarded as is", {
  expect_identical(quo(!! ~NULL), as_quosure(~NULL))
  expect_identical(quo(!! quo(NULL)), new_quosure(NULL, empty_env()))
  expect_identical(dots_quos(!! ~10L), set_names(quos_list(as_quosure(~10L)), ""))
})
