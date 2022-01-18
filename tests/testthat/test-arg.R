test_that("matches arg", {
  expect_equal(
    arg_match_wrapper("foo", c("bar", "foo")),
    "foo"
  )
  expect_snapshot_error(
    arg_match_wrapper("foo", c("bar", "baz"))
  )
})

test_that("gives an error with more than one arg", {
  expect_snapshot_error(
    arg_match0_wrapper(c("bar", "fun"), c("bar", "baz"))
  )
})

test_that("gives error with different than rearranged arg vs value", {
  f <- function(myarg = c("foo", "bar", "fun")) {
    arg_match(myarg, c("fun", "bar"))
  }
  expect_snapshot_error(
    f()
  )
  expect_snapshot_error(
    arg_match0_wrapper(c("foo", "foo"), c("foo", "bar"), arg_nm = "x")
  )
})

test_that("gives no error with rearranged arg vs value", {
  expect_identical(arg_match0_wrapper(rev(letters), letters), "z")

  skip_if_not_installed("withr")

  withr::with_seed(
    20200624L,
    expect_identical(arg_match0_wrapper(letters, sample(letters)), "a")
  )
})

test_that("uses first value when called with all values", {
  myarg <- c("bar", "baz")
  expect_identical(arg_match0_wrapper(myarg, c("bar", "baz")), "bar")
})

test_that("informative error message on partial match", {
  expect_error(
    arg_match0_wrapper("f", c("bar", "foo")),
    "Did you mean \"foo\"?"
  )
})

test_that("`arg_match()` has informative error messages", {
  arg_match_wrapper <- function(...) {
    arg_match0_wrapper(...)
  }

  expect_snapshot({
    (expect_error(arg_match_wrapper("continuuos", c("discrete", "continuous"), "my_arg")))
    (expect_error(arg_match_wrapper("fou", c("bar", "foo"), "my_arg")))
    (expect_error(arg_match_wrapper("fu", c("ba", "fo"), "my_arg")))
    (expect_error(arg_match_wrapper("baq", c("foo", "baz", "bas"), "my_arg")))
    (expect_error(arg_match_wrapper("", character(), "my_arg")))
    (expect_error(arg_match_wrapper("fo", "foo", quote(f()))))
  })
})

test_that("`arg_match()` provides no suggestion when the edit distance is too large", {
  expect_snapshot({
    (expect_error(arg_match0_wrapper("foobaz", c("fooquxs", "discrete"), "my_arg")))
    (expect_error(arg_match0_wrapper("a", c("b", "c"), "my_arg")))
  })
})

test_that("`arg_match()` finds a match even with small possible typos", {
  expect_equal(
    arg_match0_wrapper("bas", c("foo", "baz", "bas")),
    "bas"
  )
})

test_that("`arg_match()` makes case-insensitive match", {
  expect_snapshot({
    (expect_error(arg_match0_wrapper("a", c("A", "B"), "my_arg"), "Did you mean \"A\"?"))

    # Case-insensitive match is done after case-sensitive
    (expect_error(arg_match0_wrapper("aa", c("AA", "aA"), "my_arg"), "Did you mean \"aA\"?"))
  })
})

test_that("gets choices from function", {
  fn <- function(myarg = c("bar", "foo")) {
    arg_match(myarg)
  }
  expect_error(fn("f"), "Did you mean \"foo\"?")
  expect_identical(fn(), "bar")
  expect_identical(fn("foo"), "foo")
})

test_that("is_missing() works with symbols", {
  x <- missing_arg()
  expect_true(is_missing(x))
})

test_that("is_missing() works with non-symbols", {
  expect_true(is_missing(missing_arg()))

  l <- list(missing_arg())
  expect_true(is_missing(l[[1]]))
  expect_error(missing(l[[1]]), "invalid use")
})

test_that("maybe_missing() forwards missing value", {
  x <- missing_arg()
  expect_true(is_missing(maybe_missing(x)))
  expect_false(is_missing(maybe_missing(1L)))
})

test_that("is_missing() works with default arguments", {
  expect_false((function(x = 1) is_missing(x))())
  expect_false((function(x = 1) is_missing(x))(1))

  bare <- function(x) is_missing(x)
  default <- function(x = 1) is_missing(x)

  bare_bare <- function(x) bare(x)
  bare_default <- function(x) default(x)

  default_bare <- function(x = 1) bare(x)
  default_default <- function(x = 1) default(x)

  expect_true(bare())
  expect_true(bare_bare())
  expect_true(bare_default())

  expect_false(default())
  expect_false(default_bare())
  expect_false(default_default())

  expect_true(bare(missing_arg()))
  expect_true(bare_bare(missing_arg()))
  expect_true(default(missing_arg()))
  expect_true(bare_default(missing_arg()))
  expect_true(default_bare(missing_arg()))
  expect_true(default_default(missing_arg()))
})

test_that("is_missing() detects defaults that evaluate to the missing arg", {
  deprecated <- function() missing_arg()
  fn <- function(x = deprecated()) is_missing(x)
  expect_true(fn())
})

test_that("is_missing() works with dots", {
  expect_true((function(...) is_missing(..1))())
  expect_false((function(...) is_missing(..1))(1))
})

test_that("is_missing() works with enclosed arguments (currently doesn't)", {
  clo <- (function(other = 1) function() is_missing(other))()
  expect_false(clo())

  # FIXME: Probably none of these should be errors

  clo <- (function(other) function() is_missing(other))()
  expect_error(clo())
  #> ! 'missing' can only be used for arguments

  is_missing2 <- function(x) is_missing(x)

  clo <- (function(other = 1) function() is_missing2(other))()
  expect_false(clo())

  clo <- (function(other) function() is_missing2(other))()
  expect_error(clo())
  #> ! argument "other" is missing, with no default
})

test_that("is_missing() in child envs", {
  # FIXME: Should not be an error
  f <- function(x) local(is_missing(x))
  expect_error(f())
  #> ! argument "x" is missing, with no default

  f <- function(x = 1) local(is_missing(x))
  expect_false(f())
})

test_that("is_missing() is transitive", {
  caller <- function(y) f(y)

  f <- function(x = y, y = "foo") is_missing(x)
  expect_false(f())
  expect_true(caller())

  f <- function(x = y, y) is_missing(x)
  expect_true(f())
  expect_true(caller())

  f <- function(x = y, y = deprecated()) is_missing(x)
  expect_true(f())
  expect_true(caller())
})

test_that("is_missing() works in unframed envs", {
  expect_false(inject(is_missing(foo), env(foo = 2)))
  expect_true(inject(is_missing(foo), env(foo = missing_arg())))

  # Should not be an error, see previous test
  expect_error((function(x) inject(is_missing(x), env()))())
  #> ! 'missing' can only be used for arguments
})

test_that("check_required() checks argument is supplied (#1118)", {
  f <- function(x) check_required(x)
  g <- function(y) f(y)

  expect_error(f(NULL), NA)
  expect_error(g(NULL), NA)

  expect_snapshot({
    (expect_error(f()))
    (expect_error(g()))
  })
})

test_that("arg_match() supports symbols and scalar strings", {
  expect_equal(
    arg_match0_wrapper(chr_get("foo", 0L), c("bar", "foo"), "my_arg"),
    "foo"
  )
  expect_equal(
    arg_match0_wrapper(sym("foo"), c("bar", "foo"), "my_arg"),
    "foo"
  )

  expect_snapshot({
    (expect_error(arg_match0_wrapper(chr_get("fo", 0L), c("bar", "foo"), "my_arg")))
  })
})

test_that("arg_match() requires an argument symbol", {
  wrapper <- function() arg_match("foo")
  expect_snapshot((expect_error(wrapper())))
})

test_that("can match multiple arguments", {
  my_wrapper <- function(my_arg = c("foo", "bar", "baz")) {
    arg_match(my_arg, multiple = TRUE)
  }

  expect_equal(my_wrapper("foo"), "foo")
  expect_equal(my_wrapper(c("foo", "baz")), c("foo", "baz"))
  expect_equal(my_wrapper(chr()), chr())

  expect_snapshot({
    (expect_error(my_wrapper("ba")))
    (expect_error(my_wrapper(c("foo", "ba"))))
  })
})

test_that("arg_match0() defuses argument", {
  fn <- function(arg) arg_match0(arg, c("bar", "baz"))
  expect_snapshot({
    (expect_error(fn("foo")))
    (expect_error(arg_match0("foo", c("bar", "baz"))))
  })
})

test_that("check_exclusive works", {
  f <- function(foo) check_exclusive(foo)
  g <- function() check_exclusive()
  h <- function() check_exclusive(foo())

  # Internal errors
  expect_snapshot({
    (expect_error(f()))
    (expect_error(g()))
    (expect_error(h()))
  })

  f <- function(foo, bar = NULL, ...) check_exclusive(foo, bar, ...)
  g <- function(foo, bar = NULL, baz, ...) check_exclusive(foo, bar, baz, ...)

  # Zero arguments supplied
  expect_snapshot({
    (expect_error(f()))
  })
  expect_equal(f(.require = FALSE), "")

  # One argument supplied
  expect_equal(f(NULL), "foo")
  expect_equal(f(, NULL), "bar")

  # Multiple arguments supplied
  expect_snapshot({
    "All arguments supplied"
    (expect_error(g(foo, bar, baz)))

    "Some arguments supplied"
    (expect_error(g(foo, bar)))
  })
})
