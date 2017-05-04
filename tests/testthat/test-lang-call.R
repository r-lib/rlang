context("lang-call")

# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(lang(letters), "must be a length 1 string")
})

test_that("args can be specified individually or as list", {
  out <- lang("f", a = 1, splice(list(b = 2)))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

test_that("creates namespaced calls", {
  expect_identical(lang("fun", foo = quote(baz), .ns = "bar"), quote(bar::fun(foo = baz)))
})

test_that("fails with non-callable objects", {
  expect_error(lang(1), "non-callable")
  expect_error(lang(get_env()), "non-callable")
})

test_that("succeeds with literal functions", {
  expect_error(regex = NA, lang(base::mean, 1:10))
  expect_error(regex = NA, lang(base::list, 1:10))
})


# Standardisation ---------------------------------------------------------

test_that("can standardise call frame", {
  fn <- function(foo = "bar") lang_standardise(call_frame())
  expect_identical(fn(), quote(fn()))
  expect_identical(fn("baz"), quote(fn(foo = "baz")))
})

test_that("can modify call frame", {
  fn <- function(foo = "bar") lang_modify(call_frame(), baz = "bam", .standardise = TRUE)
  expect_identical(fn(), quote(fn(baz = "bam")))
  expect_identical(fn("foo"), quote(fn(foo = "foo", baz = "bam")))
})


# Modification ------------------------------------------------------------

test_that("can modify formulas inplace", {
  expect_identical(lang_modify(~matrix(bar), quote(foo)), ~matrix(bar, foo))
})

test_that("optional standardisation", {
  expect_identical(lang_modify(~matrix(bar), quote(foo), .standardise = TRUE), ~matrix(data = bar, foo))
})

test_that("new args inserted at end", {
  call <- quote(matrix(1:10))
  out <- lang_modify(call, nrow = 3, .standardise = TRUE)
  expect_equal(out, quote(matrix(data = 1:10, nrow = 3)))
})

test_that("new args replace old", {
  call <- quote(matrix(1:10))
  out <- lang_modify(call, data = 3, .standardise = TRUE)
  expect_equal(out, quote(matrix(data = 3)))
})

test_that("can modify calls for primitive functions", {
  expect_identical(lang_modify(~list(), foo = "bar", .standardise = TRUE), ~list(foo = "bar"))
})

test_that("can modify calls for functions containing dots", {
  expect_identical(lang_modify(~mean(), na.rm = TRUE, .standardise = TRUE), ~mean(na.rm = TRUE))
})

test_that("accepts unnamed arguments", {
  expect_identical(
    lang_modify(~get(), "foo", envir = "bar", "baz", .standardise = TRUE),
    ~get(envir = "bar", "foo", "baz")
  )
})

test_that("fails with duplicated arguments", {
  expect_error(lang_modify(~mean(), na.rm = TRUE, na.rm = FALSE), "Duplicate arguments")
  expect_error(lang_modify(~mean(), TRUE, FALSE), NA)
})


# Utils --------------------------------------------------------------

test_that("lang_name() handles namespaced and anonymous calls", {
  expect_equal(lang_name(quote(foo::bar())), "bar")
  expect_equal(lang_name(quote(foo:::bar())), "bar")

  expect_null(lang_name(quote(foo@bar())))
  expect_null(lang_name(quote(foo$bar())))
  expect_null(lang_name(quote(foo[[bar]]())))
  expect_null(lang_name(quote(foo()())))
  expect_null(lang_name(quote(foo::bar()())))
  expect_null(lang_name(quote((function() NULL)())))
})

test_that("lang_name() handles formulas and frames", {
  expect_identical(lang_name(~foo(baz)), "foo")

  fn <- function() lang_name(call_frame())
  expect_identical(fn(), "fn")
})

test_that("lang_fn() extracts function", {
  fn <- function() lang_fn(call_frame())
  expect_identical(fn(), fn)

  expect_identical(lang_fn(~matrix()), matrix)
})

test_that("Inlined functions return NULL name", {
  call <- quote(fn())
  call[[1]] <- function() {}
  expect_null(lang_name(call))
})

test_that("lang_args() and lang_args_names()", {
  expect_identical(lang_args(~fn(a, b)), set_names(list(quote(a), quote(b)), c("", "")))

  fn <- function(a, b) lang_args_names(call_frame())
  expect_identical(fn(a = foo, b = bar), c("a", "b"))
})
