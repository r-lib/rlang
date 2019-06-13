context("quo")

test_that("quo_get_expr() and quo_get_env() retrieve quosure components", {
  quo <- quo(foo)
  expect_identical(quo_get_expr(quo), quote(foo))
  expect_identical(quo_get_env(quo), environment())
})

test_that("quo_set_expr() and quo_set_env() set quosure components", {
  orig <- quo()
  env <- env()

  quo <- quo_set_expr(orig, quote(foo))
  expect_identical(quo_get_expr(quo), quote(foo))
  expect_identical(quo_get_expr(orig), missing_arg())

  quo <- quo_set_env(orig, env)
  expect_identical(quo_get_env(quo), env)
  expect_identical(quo_get_env(orig), empty_env())
})

test_that("quosure getters and setters check inputs", {
  expect_error(quo_get_expr(10L), "`quo` must be a quosure")
  expect_error(quo_set_expr(10L, NULL), "`quo` must be a quosure")
  expect_error(quo_get_env(10L), "`quo` must be a quosure")
  expect_error(quo_set_env(10L, env()), "`quo` must be a quosure")
  expect_error(quo_set_env(quo(), 10L), "`env` must be an environment")
})

test_that("generic getters work on quosures", {
  expect_identical(get_expr(quo(foo)), quote(foo))
  expect_identical(get_env(quo(foo)), environment())
})

test_that("generic setters work on quosures", {
  orig <- quo()
  env <- env()
  quo <- set_env(set_expr(orig, quote(foo)), env)
  expect_identical(quo_get_expr(quo), quote(foo))
  expect_identical(quo_get_env(quo), env)
})

test_that("can flatten empty quosure", {
  expect_identical(quo_squash(quo()), missing_arg())
})

test_that("new_quosure() checks inputs", {
  expect_error(new_quosure(quote(a), env = list()), "must be an environment")
})

test_that("new_quosure() produces expected internal structure", {
  quo <- new_quosure(quote(abc))
  expect_identical(structure(~abc, class = c("quosure", "formula")), quo)
})

test_that("new_quosure() double wraps", {
  quo1 <- quo(foo)
  quo2 <- new_quosure(quo1)
  expect_identical(quo_get_expr(quo2), quo1)
})

test_that("as_quosure() uses correct env", {
  fn <- function(expr, env = caller_env()) {
    f <- as_quosure(expr, env)
    list(env = current_env(), quo = g(f))
  }
  g <- function(expr, env = caller_env()) {
    as_quosure(expr, env)
  }
  quo_env <- child_env(NULL)
  quo <- new_quosure(quote(expr), quo_env)

  out_expr_default <- fn(quote(expr))
  out_quo_default <- fn(quo)
  expect_identical(quo_get_env(out_expr_default$quo), current_env())
  expect_identical(quo_get_env(out_quo_default$quo), quo_env)

  user_env <- child_env(NULL)
  out_expr <- fn(quote(expr), user_env)
  out_quo <- fn(quo, user_env)
  expect_identical(quo_get_env(out_expr$quo), user_env)
  expect_identical(out_quo$quo, quo)
})

test_that("explicit promise makes a formula", {
  capture <- function(x) enquo(x)
  f1 <- capture(1 + 2 + 3)
  f2 <- ~ 1 + 2 + 3

  expect_equal(f1, f2)
})

test_that("explicit promise works only one level deep", {
  f <- function(x) list(env = current_env(), f = g(x))
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

test_that("quosures are spliced", {
  q <- quo(foo(!! quo(bar), !! quo(baz(!! quo(baz), 3))))
  expect_identical(quo_text(q), "foo(bar, baz(baz, 3))")

  q <- expr_interp(~foo::bar(!! function(x) ...))
  expect_identical(f_text(q), "foo::bar(function (x) \n...)")

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

test_that("quo_squash() warns", {
  expect_warning(regex = NA, quo_squash(quo(foo), warn = TRUE))
  expect_warning(quo_squash(quo(list(!! quo(foo))), warn = TRUE), "inner quosure")
})

test_that("quo_deparse() indicates quosures with `^`", {
  x <- quo(list(!! quo(NULL), !! quo(foo())))
  ctxt <- new_quo_deparser(crayon = FALSE)
  expect_identical(quo_deparse(x, ctxt), "^list(^NULL, ^foo())")
})

test_that("quosure deparser respects width", {
  x <- quo(foo(quo(!!quo(bar))))
  expect_identical(length(quo_deparse(x, new_quo_deparser(width = 8L))), 3L)
  expect_identical(length(quo_deparse(x, new_quo_deparser(width = 9L))), 2L)
})

test_that("quosure predicates work", {
  expect_true(quo_is_missing(quo()))
  expect_true(quo_is_symbol(quo(sym), "sym"))
  expect_false(quo_is_symbol(quo(sym), "foo"))

  expect_true(quo_is_call(quo(call())))
  expect_true(quo_is_call(quo(ns::call()), "call", 0L, "ns"))
  expect_false(quo_is_call(quo(ns::call()), "call", 1L, "ns"))

  expect_true(quo_is_symbolic(quo(sym)))
  expect_true(quo_is_symbolic(quo(call())))
  expect_true(quo_is_null(quo(NULL)))

  expect_false(quo_is_missing(quo(10L)))
  expect_false(quo_is_symbol(quo(10L)))
  expect_false(quo_is_call(quo(10L)))
  expect_false(quo_is_symbolic(quo(10L)))
  expect_false(quo_is_symbolic(quo(10L)))
  expect_false(quo_is_null(quo(10L)))
})

test_that("new_quosures() checks that elements are quosures", {
  expect_error(new_quosures(list(1)), "list of quosures")
})

test_that("new_quosures() and as_quosures() return named lists", {
  exp <- structure(list(), names = chr(), class = "quosures")
  expect_identical(new_quosures(list()), exp)
  expect_identical(as_quosures(list()), exp)
})

test_that("as_quosures() applies default environment", {
  out <- as_quosures(list(quote(foo), quote(bar)), env = base_env())
  exp <- quos_list(new_quosure(quote(foo), base_env()), new_quosure(quote(bar), base_env()))
  expect_identical(out, exp)
})

test_that("as_quosures() auto-names if requested", {
  x <- list(quote(foo), quote(bar))
  expect_named(as_quosures(x, global_env(), named = TRUE), c("foo", "bar"))
})

test_that("quosures class has subset assign methods", {
  scoped_options(lifecycle_verbose_soft_deprecation = TRUE)

  x <- quos(1, 2)

  x[1:2] <- list(quo(3), quo(4))
  expect_identical(x, quos(3, 4))
  expect_warning(x[2] <- list(4), "deprecated")
  ## expect_error(x[2] <- list(4), "Can't assign a double vector to a list of quosures")

  x[[2]] <- quo(10)
  expect_identical(x, quos(3, 10))
  ## expect_error(x[[2]] <- list(4), "Can't assign a list to a list of quosures")

  x <- quos(foo = 1, bar = 2)

  x$bar <- quo(100)
  expect_identical(x, quos(foo = 1, bar = 100))
  ## expect_error(x$foo <- list(4), "Can't assign a list to a list of quosures")
})

test_that("can remove quosures by assigning NULL", {
  x <- quos(1, b = 2)

  x[[1]] <- NULL
  expect_identical(x, quos(b = 2))

  x$b <- NULL
  expect_identical(x, quos())
})

test_that("can't cast a quosure to base types (#523)", {
  scoped_options(lifecycle_verbose_soft_deprecation = TRUE)
  expect_warning(as.character(quo(foo)), "`as.character\\(\\)` on a quosure")
  expect_identical(as.character(quo(foo)), c("~", "foo"))
})

test_that("quosures fail with common operations (#478, tidyverse/dplyr#3476)", {
  q <- quo(NULL)

  expect_error(q + 10, "!!myquosure \\+ rhs")
  expect_error(q > q, "!!myquosure1 > !!myquosure2")
  expect_error(10 == q, "lhs == !!myquosure")

  expect_error(abs(q), "abs\\(!!myquosure\\)")
  expect_error(mean(q), "mean\\(!!myquosure\\)")
  expect_error(stats::median(q), "median\\(!!myquosure\\)")
  expect_error(stats::quantile(q), "quantile\\(!!myquosure\\)")

  expect_error(-q, "-!!myquosure")
  expect_error(-q, "+!!myquosure")
})

test_that("negating quosure fails with informative message", {
  expect_error(!quo(), "can only be unquoted within a quasiquotation")
})

test_that("can cast quosure lists to bare lists", {
  expect_identical(as.list(quos(a)), named_list(quo(a)))
})

test_that("can concatenate quosure lists", {
  expect_identical(c(quos(a, b), quos(foo = c)), quos(a, b, foo = c))
})

test_that("new_quosure() checks input", {
  expect_error(new_quosure(NULL, NULL), "`env` must be an environment")
})

test_that("as_string(quo) produces informative error message", {
  expect_error(as_string(quo(foo)), "a `quosure/formula` object to a string")
})


# Lifecycle ----------------------------------------------------------

test_that("as_quosure() still provides default env", {
  scoped_lifecycle_warnings()
  quo <- expect_warning(as_quosure(quote(foo)), "explicit environment")
  expect_reference(quo_get_env(quo), current_env())
})

test_that("can still concatenate quosure lists and non-quosures", {
  scoped_lifecycle_silence()
  expect_identical(c(quos(foo), list(1)), named_list(quo(foo), 1))
})
