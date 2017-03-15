context("arg")

# arg_inspect -----------------------------------------------------------------

test_that("follows through dots", {
  fn <- function(...) g(...)
  g <- function(...) h(...)
  h <- function(x1, x2) arg_inspect(x2)
  info <- fn(mtcars, letters)
  expect_identical(info$expr, quote(letters))
  expect_identical(info$eval_frame, call_frame())
})

test_that("empty argument are reported", {
  fn <- function(x, y) list(info = arg_inspect(x), env = environment())
  out <- fn(, )
  info <- out$info
  expect_true(is_missing(info$expr))
  expect_identical(info$eval_frame$env, out$env)
  expect_identical(info$caller_frame$env, environment())

  g <- function(x) list(info = fn(x), env = environment())
  out <- g()
  info <- out$info$info
  expect_true(is_missing(info$expr))
  expect_identical(info$eval_frame$env, out$env)
  expect_identical(info$caller_frame$env, environment())
})

test_that("formals names are recorded", {
  fn <- function(foo) arg_inspect(foo)
  expect_equal(fn()$name, "foo")

  g <- function() fn(bar)
  expect_equal(g()$name, "foo")

  g <- function() fn(foo(bar))
  expect_equal(g()$name, "foo")
})

test_that("expression is scoped in calling env", {
  fn <- function(x) arg_inspect(x)$caller_frame$env
  g <- function(x) fn(x)

  expect_identical(g(mtcars), environment())
  expect_identical(g(list(mtcars)), environment())
})

test_that("default arguments are scoped in execution env", {
  fn <- function(x = default()) list(info = g(x), env = environment())
  g <- function(x) arg_inspect(x)
  out <- fn()
  info <- out$info
  fn_env <- out$env

  expect_identical(info$eval_frame$env, fn_env)
  expect_identical(info$caller_frame$env, environment())
  expect_equal(info$expr, quote(default()))
})

test_that("missing arguments are scoped in execution env", {
  fn <- function(x) list(info = g(x), env = environment())
  g <- function(x) arg_inspect(x)
  out <- fn()
  info <- out$info
  fn_env <- out$env

  expect_identical(info$eval_frame$env, fn_env)
  expect_identical(info$caller_frame$env, environment())
  expect_true(is_missing(info$expr))
})

test_that("arguments are scoped in calling env", {
  fn <- function() list(info = g(foo), env = environment())
  g <- function(x) h(x)
  h <- function(x) arg_inspect(x)
  out <- fn()
  info <- out$info
  fn_env <- out$env

  expect_identical(info$eval_frame$env, fn_env)
  expect_identical(info$caller_frame$env, fn_env)
  expect_equal(info$expr, quote(foo))
})

test_that("global_frame() is reported with top-level calls", {
  fn <- function(x) {
    # Emulate top-level call
    stack <- call_stack(2)
    stack[[2]] <- global_frame()
    arg_inspect_(quote(x), stack)
  }
  info <- fn(foo)

  expect_identical(info$expr, quote(foo))
  expect_identical(info$eval_frame$env, globalenv())
  expect_identical(info$caller_frame$env, globalenv())
})


# arg_missing --------------------------------------------------------

test_that("is_missing() works with symbols", {
  x <- arg_missing()
  expect_true(is_missing(x))
})

test_that("is_missing() works with non-symbols", {
  expect_true(is_missing(arg_missing()))

  l <- list(arg_missing())
  expect_true(is_missing(l[[1]]))
  expect_error(missing(l[[1]]), "invalid use")
})


# special cases ------------------------------------------------------

test_that("Recall() does not mess up arg_inspect()", {
  if (utils::packageVersion("base") < "3.3.0") {
    skip("test for Recall() depends on internal implementation")
  }

  quit <- FALSE
  fn_Recall <- function(x, y) {
    if (quit) {
      quit <<- FALSE
      list(y = arg_inspect(y), x = arg_inspect(x))
    } else {
      quit <<- TRUE
      Recall()
    }
  }
  info <- fn_Recall(y = foo(bar), bar(foo))

  expect_identical(info$x$expr, quote(bar(foo)))
  expect_identical(info$y$expr, quote(foo(bar)))
  expect_identical(info$x$eval_frame$env, environment())
  expect_identical(info$x$caller_frame$env, environment())
})

test_that("magrittr works", {
  if (utils::packageVersion("magrittr") > "1.5") {
    `%>%` <- magrittr::`%>%`
    info <- letters %>% toupper() %>% .[[1]] %>% arg_inspect()
    expect_equal(info$expr, quote(.))
    expect_equal(eval(info$expr, info$eval_frame$env), "A")

    info <- letters %>% toupper() %>% .[[1]] %>% dots_inspect()
    info <- info[[1]]
    expect_equal(info$expr, quote(.))
    expect_equal(eval(info$expr, info$eval_frame$env), "A")
  }
})


# arg_quosure -------------------------------------------------------

test_that("explicit promise makes a formula", {
  capture <- function(x) arg_quosure(x)
  f1 <- capture(1 + 2 + 3)
  f2 <- ~ 1 + 2 + 3

  expect_equal(f1, f2)
})

test_that("explicit promise works only one level deep", {
  f <- function(x) list(env = get_env(), f = g(x))
  g <- function(y) arg_quosure(y)
  out <- f(1 + 2 + 3)
  expected_f <- with_env(out$env, ~x)

  expect_identical(out$f, expected_f)
})
