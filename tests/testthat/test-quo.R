context("quo")

test_that("explicit promise makes a formula", {
  capture <- function(x) enquo(x)
  f1 <- capture(1 + 2 + 3)
  f2 <- ~ 1 + 2 + 3

  expect_equal(f1, f2)
})

test_that("explicit promise works only one level deep", {
  f <- function(x) list(env = get_env(), f = g(x))
  g <- function(y) enquo(y)
  out <- f(1 + 2 + 3)
  expected_f <- with_env(out$env, quo(x))

  expect_identical(out$f, expected_f)
})

test_that("can capture optimised constants", {
  arg <- function() {
    quo("foobar")
  }
  arg_bytecode <- compiler::cmpfun(arg)

  expect_identical(arg(), quo("foobar"))
  expect_identical(arg_bytecode(), quo("foobar"))

  dots <- function() {
    quos("foo", "bar")
  }
  dots_bytecode <- compiler::cmpfun(dots)

  expect_identical(dots(), quos("foo", "bar"))
  expect_identical(dots_bytecode(), quos("foo", "bar"))
})

test_that("can enquo() at top-level", {
  quo <- new_quosure(quote(foo), global_env())
  expect_identical(eval_bare(quote(enquo(foo)), global_env()), quo)
})

test_that("quosures are spliced", {
  q <- quo(foo(!! quo(bar), !! quo(baz(!! quo(baz), 3))))
  expect_identical(quo_text(q), "foo(bar, baz(baz, 3))")

  q <- expr_interp(~foo::bar(!! function(x) ...))
  expect_identical(quo_text(q), "foo::bar(function (x) \n...)")

  q <- quo(!! quo(!! quo(foo(!! quo(!! quo(bar(!! quo(!! quo(!! quo(baz))))))))))
  expect_identical(quo_text(q), "foo(bar(baz))")
})

test_that("formulas are not spliced", {
  expect_identical(quo_text(quo(~foo(~bar))), "~foo(~bar)")
})

test_that("splicing does not affect original quosure", {
  f <- ~foo(~bar)
  quo_text(f)
  expect_identical(f, ~foo(~bar))
})

test_that("as_quosure() doesn't convert functions", {
  expect_identical(as_quosure(base::mean), set_env(quo(!! base::mean), empty_env()))
})

test_that("as_quosure() coerces formulas", {
  expect_identical(as_quosure(~foo), quo(foo))
})

test_that("quo_expr() warns", {
  expect_warning(regex = NA, quo_expr(quo(foo), warn = TRUE))
  expect_warning(quo_expr(quo(list(!! quo(foo))), warn = TRUE), "inner quosure")
})
