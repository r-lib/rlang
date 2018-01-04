context("tidy capture")

test_that("quos() creates quosures", {
  fs <- quos(x = 1 + 2, y = 2 + 3)
  expect_identical(fs$x, as_quosure(~ 1 + 2))
  expect_identical(fs$y, as_quosure(~ 2 + 3))
})

test_that("quos() captures correct environment", {
  fn <- function(x = a + b, ...) {
    list(dots = quos(x = x, y = a + b, ...), env = environment())
  }
  out <- fn(z = a + b)

  expect_identical(get_env(out$dots$x), out$env)
  expect_identical(get_env(out$dots$y), out$env)
  expect_identical(get_env(out$dots$z), get_env())
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
    expect_identical(names(quos(!!var := NULL)), "NA")

    var <- NULL
    expect_error(quos(!!var := NULL), "must be a string or a symbol")
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
})

test_that("Can't supply both `=` and `:=`", {
  expect_error(regexp = "both `=` and `:=`", quos(foobar = !!"nm" := 2))
  expect_error(regexp = "both `=` and `:=`", quos(foobar = !!"nm" := 2, .named = TRUE))
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
  expect_true(is_missing(quo_get_expr(q)))
  expect_identical(quo_get_env(q), empty_env())
})

test_that("empty quosures are forwarded", {
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

test_that("can take forced arguments", {
  fn <- function(allow, x) {
    force(x)
    captureArgInfo(x)
  }
  expect_identical(fn(TRUE, letters), list(expr = letters, env = empty_env()))

  if (getRversion() < "3.2.0") {
    skip("lapply() does not force arguments in R 3.1")
  }
  expect_error(lapply(1:2, captureArgInfo), "must be an argument name")

  args <- list(list(expr = 1L, env = empty_env()), list(expr = 2L, env = empty_env()))
  expect_identical(lapply(1:2, function(x) captureArgInfo(x)), args)
})

test_that("capturing an argument that doesn't exist fails", {
  y <- "a"

  fn <- function(x) captureArgInfo(y)
  expect_error(fn(), "object 'y' not found")

  fn <- function() enquo(y)
  expect_error(fn(), "not found")

  fn <- function() enexpr(y)
  expect_error(fn(), "not found")

  expect_error((function() rlang::enexpr(y))(), "not found")
})

test_that("can capture arguments that do exist", {
  fn <- function() {
    x <- 10L
    captureArgInfo(x)
  }
  expect_identical(fn(), list(expr = 10L, env = empty_env()))
})

test_that("can capture missing argument", {
  expect_identical(captureArgInfo(), list(expr = missing_arg(), env = empty_env()))
})

test_that("serialised unicode in `:=` LHS is unserialised", {
  nms <- with_latin1_locale({
    exprs <- exprs("\u5e78" := 10)
    names(exprs)
  })
  expect_identical(as_bytes(nms), as_bytes("\u5e78"))
})

test_that("exprs() supports auto-naming", {
  expect_identical(exprs(foo(bar), b = baz(), .named = TRUE), list(`foo(bar)` = quote(foo(bar)), b = quote(baz())))
})

test_that("dots_interp() supports unquoting", {
  expect_identical(exprs(!!(1 + 2)), named_list(3))
  expect_identical(exprs(!!(1 + 1) + 2), named_list(quote(2 + 2)))
  expect_identical(exprs(!!(1 + 1) + 2 + 3), named_list(quote(2 + 2 + 3)))
  expect_identical(exprs(!!"foo" := bar), named_list(foo = quote(bar)))
})

test_that("dots_interp() has no side effect", {
  f <- function(x) exprs(!! x + 2)
  expect_identical(f(1), named_list(quote(1 + 2)))
  expect_identical(f(2), named_list(quote(2 + 2)))
})

test_that("exprs() handles forced arguments", {
  if (getRversion() < "3.2.0") {
    skip("lapply() does not force arguments in R 3.1")
  }
  exprs <- list(named_list(1L), named_list(2L))
  expect_identical(lapply(1:2, function(...) exprs(...)), exprs)
  expect_identical(lapply(1:2, exprs), exprs)
})

test_that("quos() handles forced arguments", {
  if (getRversion() < "3.2.0") {
    skip("lapply() does not force arguments in R 3.1")
  }
  quos <- list(quos_list(quo(1L)), quos_list(quo(2L)))
  expect_identical(lapply(1:2, function(...) quos(...)), quos)
  expect_identical(lapply(1:2, quos), quos)
})

test_that("enexpr() and enquo() handle forced arguments", {
  foo <- "foo"
  expect_identical(enexpr(foo), "foo")
  expect_identical(enquo(foo), quo("foo"))

  if (getRversion() < "3.2.0") {
    skip("lapply() does not force arguments in R 3.1")
  }
  expect_identical(lapply(1:2, function(x) enexpr(x)), list(1L, 2L))
  expect_identical(lapply(1:2, function(x) enquo(x)), list(quo(1L), quo(2L)))
})

test_that("default arguments are properly captured (#201)", {
  fn <- function(x = x) enexpr(x)
  expect_identical(fn(), quote(x))

  # This is just for consistency. This causes an infinite recursion
  # when evaluated as Hong noted
  fn <- function(x = x) list(enquo(x), quo(x))
  out <- fn()
  expect_identical(out[[1]], out[[2]])
})

test_that("names-unquoting can be switched off", {
  foo <- "foo"
  bar <- "bar"

  expect_identical(exprs(foo := bar, .unquote_names = FALSE), named_list(quote(foo := bar)))
  expect_identical(exprs(!! foo := !! bar, .unquote_names = FALSE), named_list(quote("foo" := "bar")))

  expect_identical(quos(foo := bar, .unquote_names = FALSE), quos_list(new_quosure(quote(foo := bar))))
  expect_identical(quos(!! foo := !! bar, .unquote_names = FALSE), quos_list(new_quosure(quote("foo" := "bar"))))
})

test_that("enquos() captures arguments", {
  fn <- function(foo, ..., bar) enquos(foo, bar, ...)
  expect_identical(fn(arg1, arg2, bar = arg3()), quos(arg1, arg3(), arg2))
})

test_that("enquos() returns a named list", {
  fn <- function(foo, bar) enquos(foo, bar)
  expect_identical(names(fn()), c("", ""))
})

test_that("enquos() captures missing arguments", {
  fn <- function(foo) enquos(foo)[[1]]
  expect_identical(fn(), quo())

  fn <- function(...) enquos(...)
  expect_identical(fn(), quos())
})
