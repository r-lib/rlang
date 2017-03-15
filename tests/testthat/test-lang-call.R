context("lang-call")

# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(new_language(letters), "must be length 1")
})

test_that("args can be specified individually or as list", {
  out <- new_language("f", a = 1, .args = list(b = 2))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

# Standardisation ---------------------------------------------------------

test_that("args are partial matched", {
  out <- lang_homogenise(quote(matrix(nro = 3, 1:9)))
  expect_equal(out, quote(matrix(nrow = 3, data = 1:9)))
})

test_that("args are standardised", {
  f <- function(x, y) NULL
  expect_equal(lang_homogenise(~f(3)), quote(f(x = 3)))
  expect_equal(lang_homogenise(~f(3, 3)), quote(f(x = 3, y = 3)))
  expect_equal(lang_homogenise(~f(y = 3)), quote(f(y = 3)))
})

test_that("names of dotted arguments are enumerated", {
  g <- function(dots, ...) f(dots = dots, ...)
  f <- function(dots, ...) lang_homogenise(enum_dots = dots)

  f_default <- f(FALSE, foo, foo = bar, , "foobar")
  f_enum <- f(TRUE, foo, foo = bar, , "foobar")
  expect_equal(f_default, quote(f(dots = FALSE, foo, foo = bar, , "foobar")))
  expect_equal(f_enum, quote(f(dots = TRUE, ..1 = foo, ..2 = bar, ..3 = , ..4 = "foobar")))

  g_default <- g(FALSE, foo, foo = bar, , "foobar")
  g_enum <- g(TRUE, foo, foo = bar, , "foobar")
  expect_equal(g_default, quote(f(dots = dots, ..1, foo = ..2, ..3, ..4)))
  expect_equal(g_enum, quote(f(dots = dots, ..1 = ..1, ..2 = ..2, ..3 = ..3, ..4 = ..4)))

  enum <- FALSE
  f <- function(...) h(...)
  h <- function(x1, x2) lang_homogenise(enum_dots = enum)
  expect_equal(f(a, b), quote(h(x1 = ..1, x2 = ..2)))

  enum <- TRUE
  expect_equal(f(a, b), quote(h(x1 = ..1, x2 = ..2)))
})

test_that("call is not modified in place", {
  f <- function(...) g(...)
  g <- function(...) call_stack()[1:2]
  stack <- f(foo)
  expect_equal(stack[[1]]$expr, quote(g(...)))
})

test_that("can standardise without specifying `call`", {
  f <- function(...) lang_homogenise()
  expect_identical(f(arg), quote(f(arg)))
})

test_that("can standardise formulas", {
  f <- matrix
  expect_equal(lang_homogenise(~f(x, y, z)), quote(f(data = x, nrow = y, ncol = z)))
})

test_that("empty dots do not throw an error", {
  f <- function() g()
  g <- function(...) h(...)
  h <- function(...) lang_homogenise(enum_dots = TRUE)
  expect_error(f(), NA)
})

test_that("matching dots without caller_env throws", {
  fn <- function(...) lang_homogenise(quote(fn(...)))
  g <- function(...) fn(...)
  expect_error(g(), "must be supplied to match dots")
})

test_that("arguments are partially matched", {
  fn <- function(abc, abcd) NULL
  expect_equal(call_match_partial(quote(fn()), fn), quote(fn()))
  expect_equal(call_match_partial(quote(fn(ab = 1, abc = 2)), fn), quote(fn(abcd = 1, abc = 2)))
  expect_equal(call_match_partial(quote(matrix(nro = 3, 1:9)), matrix), quote(matrix(nrow = 3, 1:9)))
})

test_that("multiple formal matches throw", {
  fn <- function(abc, abcd) NULL
  expect_error(call_match_partial(quote(fn(a = 1, a = 2)), fn), "matched by multiple")
  expect_error(call_match_partial(quote(fn(a = 1, ab = 2)), fn), "matched by multiple")
})

test_that("multiple actual matches throw", {
  fn <- function(abc, abcd) NULL
  expect_error(call_match_partial(quote(fn(a = 2)), fn), "matches multiple")
})

test_that("redundant args throw", {
  fn <- function(x, y) NULL
  expect_error(lang_homogenise(quote(fn(x = NULL, x = NULL))), "matched by multiple")
  expect_error(lang_homogenise(quote(fn(NULL, NULL, NULL))), "unused argument")
})

test_that("unused args throw", {
  fn <- function(x, y) NULL
  expect_error(lang_homogenise(~fn(x = 1, z = 2)), "unused arguments: z")
})

test_that("multiple matches are allowed within dots", {
  fn <- function(x, ...) lang_homogenise()
  expect_error(fn(x = 1, x = 2), "matched by multiple actual")

  fn <- function(...) lang_homogenise()
  expect_equal(fn(x = 1, x = 2), quote(fn(x = 1, x = 2)))
})

test_that("multiple dots are allowed in a call?", {
  # Is this important to fix?
  fn <- function(...) list(lang_homogenise(), ...)
  h <- function(...) fn(..., ...)
  h(1)
})

test_that("crazy args partial-match", {
  fn <- function(`\\[]`, `[]\\`) NULL
  expect_equal(lang_homogenise(~fn(`[]` = 1, `\\` = 2)), quote(fn(`[]\\` = 1, `\\[]` = 2)))
  expect_error(lang_homogenise(~fn(`\\` = 1, `\\[` = 2)), "matched by multiple")
})

test_that("args after dots are not partial-matched", {
  fn <- function(..., abc) lang_homogenise()
  g <- function(...) fn(...)
  expect_equal(g(a = 1), quote(fn(a = ..1)))

  fn <- function(ab, ..., abc) lang_homogenise()
  g <- function(...) fn(...)
  expect_equal(g(a = 1), quote(fn(ab = ..1)))
})

test_that("enumerated dots are ignored when checking unused args", {
  fn <- function(x) lang_homogenise(enum_dots = TRUE)
  g <- function(...) fn(...)
  h <- function(...) g(...)
  expect_error(h(a), NA)
})

test_that("dots are not confused with formals", {
  enum <- TRUE
  fn <- function(x, ...) lang_homogenise(enum_dots = enum)
  expect_equal(fn(z = foo), quote(fn(..1 = foo)))
  expect_equal(fn(z = foo, bar, baz), quote(fn(..1 = foo, x = bar, ..2 = baz)))
  expect_equal(fn(z = foo, x = bar, baz), quote(fn(..1 = foo, x = bar, ..2 = baz)))

  enum <- FALSE
  expect_equal(fn(z = foo), quote(fn(z = foo)))
  expect_equal(fn(z = foo, bar, baz), quote(fn(z = foo, x = bar, baz)))
  expect_equal(fn(z = foo, bar, x = baz), quote(fn(z = foo, bar, x = baz)))

  enum <- TRUE
  fn <- function(x, y, ...) lang_homogenise(enum_dots = enum)
  expect_equal(fn(z = foo, bar, x = baz, bam), quote(fn(..1 = foo, y = bar, x = baz, ..2 = bam)))

  enum <- FALSE
  expect_equal(fn(z = foo, bar, x = baz, bam), quote(fn(z = foo, y = bar, x = baz, bam)))
})

test_that("missing arguments are matched as well", {
  fn <- function(x, y, z) lang_homogenise(add_missings = TRUE)
  expect_equal(fn(y = foo), quote(fn(y = foo, x = , z = )))
})

test_that("dots are not treated as missing arg", {
  fn <- function(x, ...) lang_homogenise(add_missings = TRUE)
  expect_equal(fn(), quote(fn(x = )))
})

test_that("global_frame() can be standardised", {
  expect_null(lang_homogenise(global_frame()))
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

test_that("can modify without supplying `call`", {
  locally({
    f <- function(std) lang_modify(.args = list(bool = FALSE), .standardise = std)
    expect_identical(f(TRUE), quote(f(std = TRUE, bool = FALSE)))
    expect_identical(f(FALSE), quote(f(FALSE, bool = FALSE)))
  })
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

  fn <- function() lang_name()
  expect_identical(fn(), "fn")
})

test_that("lang_fn() extracts function", {
  fn <- function() lang_fn()
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

  fn <- function(a, b) lang_args_names()
  expect_identical(fn(a = foo, b = bar), c("a", "b"))
})


# call_stack() consolidation -----------------------------------------

test_that("Recall() does not mess up call history", {
  if (utils::packageVersion("base") < "3.3.0") {
    skip("test for Recall() depends on internal implementation")
  }

  counter <- 2L
  fn <- function(x) {
    if (counter) {
      counter <<- counter - 1L
      Recall(x)
    } else {
      counter <<- 2L
      call_stack()
    }
  }

  stack <- fn(foo)
  trail <- map_int(stack, function(x) x$pos)
  expect_equal(fixup_call_trail(trail), 5:1)

  calls <- map(stack, lang_homogenise, enum_dots = TRUE, add_missings = TRUE)
  expected_calls <- alist(
    fn(x = ..1),
    Recall(..1 = x),
    fn(x = ..1),
    Recall(..1 = x),
    fn(x = foo)
  )
  expect_equal(calls[1:5], expected_calls)
})
