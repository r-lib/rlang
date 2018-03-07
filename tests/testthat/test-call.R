context("call")

# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(call2(letters), "must be a length 1 string")
})

test_that("args can be specified individually or as list", {
  out <- call2("f", a = 1, splice(list(b = 2)))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

test_that("creates namespaced calls", {
  expect_identical(call2("fun", foo = quote(baz), .ns = "bar"), quote(bar::fun(foo = baz)))
})

test_that("fails with non-callable objects", {
  expect_error(call2(1), "non-callable")
  expect_error(call2(current_env()), "non-callable")
})

test_that("succeeds with literal functions", {
  expect_error(regex = NA, call2(base::mean, 1:10))
  expect_error(regex = NA, call2(base::list, 1:10))
})


# Standardisation ---------------------------------------------------------

test_that("can standardise call frame", {
  fn <- function(foo = "bar") call_standardise(call_frame())
  expect_identical(fn(), quote(fn()))
  expect_identical(fn("baz"), quote(fn(foo = "baz")))
})

test_that("can modify call frame", {
  fn <- function(foo = "bar") call_modify(call_frame(), baz = "bam", .standardise = TRUE)
  expect_identical(fn(), quote(fn(baz = "bam")))
  expect_identical(fn("foo"), quote(fn(foo = "foo", baz = "bam")))
})


# Modification ------------------------------------------------------------

test_that("can modify formulas inplace", {
  expect_identical(call_modify(~matrix(bar), quote(foo)), ~matrix(bar, foo))
})

test_that("optional standardisation", {
  expect_identical(call_modify(~matrix(bar), quote(foo), .standardise = TRUE), ~matrix(data = bar, foo))
})

test_that("new args inserted at end", {
  call <- quote(matrix(1:10))
  out <- call_modify(call, nrow = 3, .standardise = TRUE)
  expect_equal(out, quote(matrix(data = 1:10, nrow = 3)))
})

test_that("new args replace old", {
  call <- quote(matrix(1:10))
  out <- call_modify(call, data = 3, .standardise = TRUE)
  expect_equal(out, quote(matrix(data = 3)))
})

test_that("can modify calls for primitive functions", {
  expect_identical(call_modify(~list(), foo = "bar", .standardise = TRUE), ~list(foo = "bar"))
})

test_that("can modify calls for functions containing dots", {
  expect_identical(call_modify(~mean(), na.rm = TRUE, .standardise = TRUE), ~mean(na.rm = TRUE))
})

test_that("accepts unnamed arguments", {
  expect_identical(
    call_modify(~get(), "foo", envir = "bar", "baz", .standardise = TRUE),
    ~get(envir = "bar", "foo", "baz")
  )
})

test_that("fails with duplicated arguments", {
  expect_error(call_modify(~mean(), na.rm = TRUE, na.rm = FALSE), "Duplicate arguments")
  expect_error(call_modify(~mean(), TRUE, FALSE), NA)
})


# Utils --------------------------------------------------------------

test_that("NULL is a valid language object", {
  expect_true(is_expression(NULL))
})

test_that("is_call() pattern-matches", {
  expect_true(is_call(quote(foo(bar)), "foo"))
  expect_false(is_call(quote(foo(bar)), "bar"))
  expect_true(is_call(quote(foo(bar)), quote(foo)))

  expect_true(is_call(quote(foo(bar)), "foo", n = 1))
  expect_false(is_call(quote(foo(bar)), "foo", n = 2))
  expect_true(is_call(quote(+3), n = 1))
  expect_true(is_call(quote(3 + 3), n = 2))

  expect_true(is_call(quote(foo::bar())), quote(foo::bar()))

  expect_false(is_call(1))
  expect_false(is_call(NULL))
})

test_that("is_call() vectorises name", {
  expect_false(is_call(quote(foo::bar), c("fn", "fn2")))
  expect_true(is_call(quote(foo::bar), c("fn", "::")))

  expect_true(is_call(quote(foo::bar), quote(`::`)))
  expect_true(is_call(quote(foo::bar), list(quote(`@`), quote(`::`))))
  expect_false(is_call(quote(foo::bar), list(quote(`@`), quote(`:::`))))
})

test_that("call_name() handles namespaced and anonymous calls", {
  expect_equal(call_name(quote(foo::bar())), "bar")
  expect_equal(call_name(quote(foo:::bar())), "bar")

  expect_null(call_name(quote(foo@bar())))
  expect_null(call_name(quote(foo$bar())))
  expect_null(call_name(quote(foo[[bar]]())))
  expect_null(call_name(quote(foo()())))
  expect_null(call_name(quote(foo::bar()())))
  expect_null(call_name(quote((function() NULL)())))
})

test_that("call_name() handles formulas and frames", {
  expect_identical(call_name(~foo(baz)), "foo")

  fn <- function() call_name(call_frame())
  expect_identical(fn(), "fn")
})

test_that("call_fn() extracts function", {
  fn <- function() call_fn(call_frame())
  expect_identical(fn(), fn)

  expect_identical(call_fn(~matrix()), matrix)
})

test_that("call_fn() looks up function in `env`", {
  env <- local({
    fn <- function() "foo"
    current_env()
  })
  expect_identical(call_fn(quote(fn()), env = env), env$fn)
})

test_that("Inlined functions return NULL name", {
  call <- quote(fn())
  call[[1]] <- function() {}
  expect_null(call_name(call))
})

test_that("call_args() and call_args_names()", {
  expect_identical(call_args(~fn(a, b)), set_names(list(quote(a), quote(b)), c("", "")))

  fn <- function(a, b) call_args_names(call_frame())
  expect_identical(fn(a = foo, b = bar), c("a", "b"))
})

test_that("qualified and namespaced symbols are recognised", {
  expect_true(is_qualified_call(quote(foo@baz())))
  expect_true(is_qualified_call(quote(foo::bar())))
  expect_false(is_qualified_call(quote(foo()())))

  expect_false(is_namespaced_call(quote(foo@bar())))
  expect_true(is_namespaced_call(quote(foo::bar())))
})

test_that("can specify ns in namespaced predicate", {
  expr <- quote(foo::bar())
  expect_false(is_namespaced_call(expr, quote(bar)))
  expect_true(is_namespaced_call(expr, quote(foo)))
  expect_true(is_namespaced_call(expr, "foo"))
})

test_that("can specify ns in is_call()", {
  expr <- quote(foo::bar())
  expect_true(is_call(expr, ns = NULL))
  expect_false(is_call(expr, ns = ""))
  expect_false(is_call(expr, ns = "baz"))
  expect_true(is_call(expr, ns = "foo"))
  expect_true(is_call(expr, name = "bar", ns = "foo"))
  expect_false(is_call(expr, name = "baz", ns = "foo"))
})

test_that("can unnamespace calls", {
  expect_identical(call_unnamespace(quote(bar(baz))), quote(bar(baz)))
  expect_identical(call_unnamespace(quote(foo::bar(baz))), quote(bar(baz)))
  expect_identical(call_unnamespace(quote(foo@bar(baz))), quote(foo@bar(baz)))
})

test_that("precedence of regular calls", {
  expect_true(call_has_precedence(quote(1 + 2), quote(foo(1 + 2))))
  expect_true(call_has_precedence(quote(foo()), quote(1 + foo())))
})

test_that("precedence of associative ops", {
  expect_true(call_has_precedence(quote(1 + 2), quote(1 + 2 + 3), "lhs"))
  expect_false(call_has_precedence(quote(2 + 3), quote(1 + 2 + 3), "rhs"))
  expect_false(call_has_precedence(quote(1^2), quote(1^2^3), "lhs"))
  expect_true(call_has_precedence(quote(2^3), quote(1^2^3), "rhs"))
})
