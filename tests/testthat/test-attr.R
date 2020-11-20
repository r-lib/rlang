context("attributes")

test_that("names2() takes care of missing values", {
  x <- set_names(1:3, c("a", NA, "b"))
  expect_identical(names2(x), c("a", "", "b"))
})

test_that("names2() fails for environments", {
  expect_error(names2(env()), "Use `env_names()` for environments.", fixed = TRUE)
})

test_that("inputs must be valid", {
  expect_error(set_names(environment()), "must be a vector")
  expect_error(set_names(1:10, letters[1:4]), "same length")
})

test_that("can supply vector or ...", {
  expect_named(set_names(1:2, c("a", "b")), c("a", "b"))
  expect_named(set_names(1:2, "a", "b"), c("a", "b"))
  expect_named(set_names(1:2, 1, 2), c("1", "2"))
})

test_that("can supply function/formula to rename", {
  x <- c(a = 1, b = 2)
  expect_named(set_names(x, toupper), c("A", "B"))
  expect_named(set_names(x, ~ toupper(.)), c("A", "B"))
  expect_named(set_names(x, paste, "foo"), c("a foo", "b foo"))
})

test_that("set_names() zaps names", {
  expect_null(names(set_names(mtcars, NULL)))
})

test_that("set_names() coerces to character", {
  expect_identical(set_names(1L, TRUE), c(`TRUE` = 1L))
  expect_identical(set_names(1:2, "a", TRUE), c(a = 1L, `TRUE` = 2L))
})

test_that("set_names() checks length generically", {
  x <- as.POSIXlt("1970-01-01", tz = "UTC")

  expect <- x
  names(expect) <- "a"

  expect_identical(set_names(x, "a"), expect)
  expect_error(set_names(x, c("a", "b")), "the same length")
})

test_that("has_name() works with pairlists", {
  expect_true(has_name(fn_fmls(`[.data.frame`), "drop"))
})

test_that("set_names() first names the vector before applying a function (#688)", {
  exp <- set_names(letters, toupper(letters))
  expect_identical(set_names(set_names(letters), toupper), exp)
  expect_identical(set_names(letters, toupper), exp)
})

test_that("set_names2() fills in empty names", {
  chr <- c("a", b = "B", "c")
  expect_equal(set_names2(chr), c(a = "a", b = "B", c = "c"))
})

test_that("zap_srcref() removes source references", {
  with_srcref("x <- quote({ NULL })")
  expect_null(attributes(zap_srcref(x)))
})

test_that("zap_srcref() handles nested functions (r-lib/testthat#1228)", {
  with_srcref("
    factory <- function() {
      function() {
        function() {
          1
        }
      }
    }"
  )

  fn <- zap_srcref(factory())
  expect_null(attributes(fn))

  curly <- body(fn)
  expect_null(attributes(curly))

  fn_call <- curly[[2]]
  expect_null(attributes(fn_call))

  # Calls to `function` store srcrefs in 4th cell
  expect_length(fn_call, 3)
})

test_that("zap_srcref() works with quosures", {
  with_srcref("x <- expr({ !!quo({ NULL }) })")

  out <- zap_srcref(x)
  expect_null(attributes(out))

  quo <- out[[2]]
  expect_null(attributes(quo_get_expr(quo)))
})
