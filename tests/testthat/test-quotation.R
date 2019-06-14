context("quotation")

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
  expect_identical(get_env(out$dots$z), current_env())
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
      env = current_env()
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

  env <- child_env(current_env())
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
  fn <- function(x) captureArgInfo(`_foobar`)
  expect_error(fn(), "object '_foobar' not found")

  fn <- function() enquo(`_foobar`)
  expect_error(fn(), "not found")

  fn <- function() enexpr(`_foobar`)
  expect_error(fn(), "not found")

  expect_error((function() rlang::enexpr(`_foobar`))(), "not found")
})

test_that("can capture arguments across ancestry", {
  y <- "foo"
  fn <- function() captureArgInfo(y)
  expect_identical(fn(), list(expr = "foo", env = empty_env()))
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

test_that("endots() captures arguments", {
  # enquos()
  fn <- function(foo, ..., bar) enquos(foo, bar, ...)
  expect_identical(fn(arg1, arg2, bar = arg3()), quos(arg1, arg3(), arg2))

  # enexprs()
  fn <- function(foo, ..., bar) enexprs(foo, bar, ...)
  expect_identical(fn(arg1, arg2, bar = arg3()), exprs(arg1, arg3(), arg2))
})

test_that("endots() requires symbols", {
  expect_error(enquos(foo(bar)), "must be argument names")
  expect_error(enquos(1), "must be argument names")
  expect_error(enquos("foo"), "must be argument names")

  expect_error(enexprs(foo(bar)), "must be argument names")
  expect_error(enexprs(1), "must be argument names")
  expect_error(enexprs("foo"), "must be argument names")
})

test_that("endots() returns a named list", {
  # enquos()
  fn <- function(foo, bar) enquos(foo, bar)
  expect_identical(names(fn()), c("", ""))
  fn <- function(arg, ...) enquos(other = arg, ...)
  expect_identical(fn(arg = 1, b = 2), quos(other = 1, b = 2))

  # enexprs()
  fn <- function(foo, bar) enexprs(foo, bar)
  expect_identical(names(fn()), c("", ""))
  fn <- function(arg, ...) enexprs(other = arg, ...)
  expect_identical(fn(arg = 1, b = 2), exprs(other = 1, b = 2))
})

test_that("endots() captures missing arguments", {
  # enquos()
  fn <- function(foo) enquos(foo)[[1]]
  expect_identical(fn(), quo())
  fn <- function(...) enquos(...)
  expect_identical(fn(), quos())

  # enexprs()
  fn <- function(foo) enexprs(foo)[[1]]
  expect_identical(fn(), expr())
  fn <- function(...) enexprs(...)
  expect_identical(fn(), exprs())
})

test_that("endots() supports `.named`", {
  # enquos()
  fn <- function(arg, ...) enquos(arg, ..., .named = TRUE)
  expect_identical(fn(foo, bar), quos(foo = foo, bar = bar))

  # enexprs()
  fn <- function(arg, ...) enexprs(arg, ..., .named = TRUE)
  expect_identical(fn(foo, bar), exprs(foo = foo, bar = bar))
})

test_that("endots() supports `.unquote_names`", {
  # enquos()
  fn <- function(...) enquos(..., .unquote_names = TRUE)
  expect_identical(fn(!!"foo" := bar), quos(foo = bar))
  fn <- function(...) enquos(..., .unquote_names = FALSE)
  expect_identical(fn(!!"foo" := bar), quos(!!"foo" := bar, .unquote_names = FALSE))

  # enexprs()
  fn <- function(...) enexprs(..., .unquote_names = TRUE)
  expect_identical(fn(!!"foo" := bar), exprs(foo = bar))
  fn <- function(...) enexprs(..., .unquote_names = FALSE)
  expect_identical(fn(!!"foo" := bar), exprs(!!"foo" := bar, .unquote_names = FALSE))
})

test_that("endots() supports `.ignore_empty`", {
  # enquos()
  fn <- function(...) enquos(..., .ignore_empty = "all")
  expect_identical(fn(, ), quos())
  fn <- function(...) enquos(..., .ignore_empty = "trailing")
  expect_identical(fn(foo, ), quos(foo))

  # enexprs()
  fn <- function(...) enexprs(..., .ignore_empty = "all")
  expect_identical(fn(, ), exprs())
  fn <- function(...) enexprs(..., .ignore_empty = "trailing")
  expect_identical(fn(foo, ), exprs(foo))
})

test_that("ensyms() captures multiple symbols", {
  fn <- function(arg, ...) ensyms(arg, ...)
  expect_identical(fn(foo, bar, baz), exprs(foo, bar, baz))
  expect_error(fn(foo()), "Only strings can be converted to symbols")
})

test_that("enquos() works with lexically scoped dots", {
  capture <- function(...) {
    eval_bare(quote(enquos(...)), child_env(env()))
  }
  expect_identical(capture("foo"), quos_list(quo("foo")))
})

test_that("enquo() works with lexically scoped arguments", {
  capture <- function(arg) {
    eval_bare(quote(enquo(arg)), child_env(env()))
  }
  expect_identical(capture(foo), quo(foo))
})

test_that("dots_definitions() uses tidy eval", {
  var1 <- "foo"
  var2 <- "bar"
  dots <- dots_definitions(pat = foo(!!var1) := bar(!!var2))
  pat <- list(lhs = quo(foo("foo")), rhs = quo(bar("bar")))
  expect_identical(dots$defs$pat, pat)
})

test_that("dots_definitions() accepts other types of arguments", {
  dots <- dots_definitions(A = a := b, B = c)
  expect_identical(dots$defs$A, list(lhs = quo(a), rhs = quo(b)))
  expect_identical(dots$dots$B, quo(c))
})

test_that("closures are captured with their calling environment", {
  expect_reference(quo_get_env(quo(!!function() NULL)), environment())
})

test_that("the missing argument is captured", {
  expect_equal_(quos(!!missing_arg()), quos(, ))

  fn <- function(x) {
    g(!!enquo(x))
  }
  g <- function(...) {
    quos(...)
  }
  expect_equal_(fn(), quos(!!missing_arg()))
})

test_that("missing names are forwarded", {
  x <- set_names(1:2, c(NA, NA))
  expect_identical_(names(exprs(!!!x)), chr(na_chr, na_chr))
})

test_that("`.named` must be integerish", {
  expect_error(exprs(foo, .named = 100.5), "must be a scalar logical")
})

test_that("auto-naming uses type_sum() (#573)", {
  expect_named(quos(foo, !!(1:3), .named = TRUE), c("foo", "<int>"))

  x <- list(env(), 1:3, letters)
  expect_named(exprs_auto_name(x), c("<env>", "<int>", "<chr>"))
})

test_that("auto-naming supports the .data pronoun", {
  exprs <- exprs(.data[[toupper("foo")]], .data$bar, .named = TRUE)
  expect_named(exprs, c("FOO", "bar"))
})

test_that("enexprs() and enquos() support `.ignore_empty = 'all'` (#414)", {
  myexprs <- function(what, x, y) enexprs(x = x, y = y, .ignore_empty = what)
  expect_identical(myexprs("none"), exprs(x = , y = ))
  expect_identical(myexprs("trailing"), exprs(x = , y = ))
  expect_identical(myexprs("all"), exprs())

  myquos <- function(what, x, y) enquos(x = x, y = y, .ignore_empty = what)
  expect_identical(myquos("none"), quos(x = , y = ))
  expect_identical(myquos("trailing"), quos(x = , y = ))
  expect_identical(myquos("all"), quos())
})

test_that("enexprs() and enquos() support empty dots", {
  myexprs <- function(what, ...) enexprs(..., .ignore_empty = what)
  expect_identical(myexprs("none"), exprs())
  expect_identical(myexprs("trailing"), exprs())
  expect_identical(myexprs("all"), exprs())

  myquos <- function(what, ...) enquos(..., .ignore_empty = what)
  expect_identical(myquos("none"), quos())
  expect_identical(myquos("trailing"), quos())
  expect_identical(myquos("all"), quos())
})

test_that("supplying `!!!` with a name warns", {
  scoped_options(lifecycle_verbose_soft_deprecation = TRUE)
  expect_no_warning_(quos(!!!1, 2, !!!NULL))
  expect_defunct(quos(foo = !!!1, 2, bar = !!!NULL), "Only the operand's names are retained")
})

test_that("ensym() unwraps quosures", {
  fn <- function(arg) ensym(arg)
  expect_identical(fn(!!quo(foo)), quote(foo))
  expect_identical(fn(!!quo("foo")), quote(foo))
  expect_error(fn(!!quo(foo())), "Only strings can be converted to symbols")
})

test_that("ensyms() unwraps quosures", {
  fn <- function(...) ensyms(...)
  expect_identical(fn(!!!quos(foo, "bar")), exprs(foo, bar))
  expect_error(fn(!!!quos(foo, bar())), "Only strings can be converted to symbols")
})
