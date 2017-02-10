context("call")

# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(new_call(letters), "must be length 1")
})

test_that("args can be specified individually or as list", {
  out <- new_call("f", a = 1, .args = list(b = 2))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

# Standardisation ---------------------------------------------------------

test_that("args are partial matched", {
  out <- call_standardise(quote(matrix(nro = 3, 1:9)))
  expect_equal(out, quote(matrix(nrow = 3, data = 1:9)))
})

test_that("args are standardised", {
  f <- function(x, y) NULL
  expect_equal(call_standardise(quote(f(3))), quote(f(x = 3)))
  expect_equal(call_standardise(quote(f(3, 3))), quote(f(x = 3, y = 3)))
  expect_equal(call_standardise(quote(f(y = 3))), quote(f(y = 3)))
})

test_that("names of dotted arguments are enumerated", {
  g <- function(dots, ...) f(dots = dots, ...)
  f <- function(dots, ...) call_standardise(enum_dots = dots)

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
  h <- function(x1, x2) call_standardise(enum_dots = enum)
  expect_equal(f(a, b), quote(h(x1 = ..1, x2 = ..2)))

  enum <- TRUE
  expect_equal(f(a, b), quote(h(x1 = ..1, x2 = ..2)))
})

test_that("call is not modified in place", {
  f <- function(...) g(...)
  g <- function(...) call_stack()[1:2]
  stack <- f(foo)
  call_standardise(stack[[1]]$expr, g, stack[[2]]$env, enum_dots = TRUE)
  expect_equal(stack[[1]]$expr, quote(g(...)))
})

test_that("can standardise without specifying `call`", {
  f <- function(...) call_standardise()
  expect_identical(f(arg), quote(f(arg)))
})

test_that("can standardise formulas", {
  f <- matrix
  expect_equal(call_standardise(~f(x, y, z)), quote(f(data = x, nrow = y, ncol = z)))
})

test_that("empty dots do not throw an error", {
  f <- function() g()
  g <- function(...) h(...)
  h <- function(...) call_standardise(enum_dots = TRUE)
  expect_error(f(), NA)
})

test_that("matching dots without caller_env throws", {
  fn <- function(...) call_standardise(quote(fn(...)))
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
  expect_error(call_standardise(quote(fn(x = NULL, x = NULL))), "matched by multiple")
  expect_error(call_standardise(quote(fn(NULL, NULL, NULL))), "unused argument")
})

test_that("unused args throw", {
  fn <- function(x, y) NULL
  expect_error(call_standardise(~fn(x = 1, z = 2)), "unused arguments: z")
})

test_that("multiple matches are allowed within dots", {
  fn <- function(x, ...) call_standardise()
  expect_error(fn(x = 1, x = 2), "matched by multiple actual")

  fn <- function(...) call_standardise()
  expect_equal(fn(x = 1, x = 2), quote(fn(x = 1, x = 2)))
})

test_that("multiple dots are allowed in a call?", {
  # Is this important to fix?
  fn <- function(...) list(call_standardise(), ...)
  h <- function(...) fn(..., ...)
  h(1)
})

test_that("crazy args partial-match", {
  fn <- function(`\\[]`, `[]\\`) NULL
  expect_equal(call_standardise(~fn(`[]` = 1, `\\` = 2)), quote(fn(`[]\\` = 1, `\\[]` = 2)))
  expect_error(call_standardise(~fn(`\\` = 1, `\\[` = 2)), "matched by multiple")
})

test_that("args after dots are not partial-matched", {
  fn <- function(..., abc) call_standardise()
  g <- function(...) fn(...)
  expect_equal(g(a = 1), quote(fn(a = ..1)))

  fn <- function(ab, ..., abc) call_standardise()
  g <- function(...) fn(...)
  expect_equal(g(a = 1), quote(fn(ab = ..1)))
})

test_that("enumerated dots are ignored when checking unused args", {
  fn <- function(x) call_standardise(enum_dots = TRUE)
  g <- function(...) fn(...)
  h <- function(...) g(...)
  expect_error(h(a), NA)
})

test_that("dots are not confused with formals", {
  enum <- TRUE
  fn <- function(x, ...) call_standardise(enum_dots = enum)
  expect_equal(fn(z = foo), quote(fn(..1 = foo)))
  expect_equal(fn(z = foo, bar, baz), quote(fn(..1 = foo, x = bar, ..2 = baz)))
  expect_equal(fn(z = foo, x = bar, baz), quote(fn(..1 = foo, x = bar, ..2 = baz)))

  enum <- FALSE
  expect_equal(fn(z = foo), quote(fn(z = foo)))
  expect_equal(fn(z = foo, bar, baz), quote(fn(z = foo, x = bar, baz)))
  expect_equal(fn(z = foo, bar, x = baz), quote(fn(z = foo, bar, x = baz)))

  enum <- TRUE
  fn <- function(x, y, ...) call_standardise(enum_dots = enum)
  expect_equal(fn(z = foo, bar, x = baz, bam), quote(fn(..1 = foo, y = bar, x = baz, ..2 = bam)))

  enum <- FALSE
  expect_equal(fn(z = foo, bar, x = baz, bam), quote(fn(z = foo, y = bar, x = baz, bam)))
})

test_that("missing arguments are matched as well", {
  fn <- function(x, y, z) call_standardise(add_missings = TRUE)
  expect_equal(fn(y = foo), quote(fn(y = foo, x = , z = )))
})

test_that("dots are not treated as missing arg", {
  fn <- function(x, ...) call_standardise(add_missings = TRUE)
  expect_equal(fn(), quote(fn(x = )))
})

test_that("global_frame() can be standardised", {
  expect_null(call_standardise(global_frame()))
})


# Modification ------------------------------------------------------------

test_that("all args must be named", {
  call <- quote(matrix(1:10))
  expect_error(call_modify(call, 1), "must be named")
})

test_that("new args inserted at end", {
  call <- quote(matrix(1:10))
  out <- call_modify(call, nrow = 3)
  expect_equal(out, quote(matrix(data = 1:10, nrow = 3)))
})

test_that("new args replace old", {
  call <- quote(matrix(1:10))
  out <- call_modify(call, data = 3)
  expect_equal(out, quote(matrix(data = 3)))
})

test_that("can modify without supplying `call`", {
  f <- function() call_modify(.args = list(bool = FALSE))
  expect_identical(f(), quote(f(bool = FALSE)))
})

# Utils --------------------------------------------------------------

test_that("call_fn_name() handles namespaced and anonymous calls", {
  expect_equal(call_fn_name(quote(foo::bar())), "bar")
  expect_equal(call_fn_name(quote(foo:::bar())), "bar")

  expect_null(call_fn_name(quote(foo@bar())))
  expect_null(call_fn_name(quote(foo$bar())))
  expect_null(call_fn_name(quote(foo[[bar]]())))
  expect_null(call_fn_name(quote(foo()())))
  expect_null(call_fn_name(quote(foo::bar()())))
  expect_null(call_fn_name(quote((function() NULL)())))
})

test_that("call_fn() extracts function", {
  fn <- function() call_fn()
  expect_identical(fn(), fn)
})

test_that("Inlined functions return NULL name", {
  call <- quote(fn())
  call[[1]] <- function() {}
  expect_null(call_fn_name(call))
})

test_that("call_args() and call_args_names()", {
  expect_identical(call_args(~fn(a, b)), set_names(list(quote(a), quote(b)), c("", "")))

  fn <- function(a, b) call_args_names()
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

  calls <- map(stack, call_standardise, enum_dots = TRUE, add_missings = TRUE)
  expected_calls <- alist(
    fn(x = ..1),
    Recall(..1 = x),
    fn(x = ..1),
    Recall(..1 = x),
    fn(x = foo)
  )
  expect_equal(calls[1:5], expected_calls)
})
