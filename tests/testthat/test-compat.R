context("compat")

test_that("names() dispatches on environment", {
  env <- child_env(data = list(foo = "foo", bar = "bar"))
  expect_identical(sort(names(env)), c("bar", "foo"))
})

test_that("lazy objects are converted to tidy quotes", {
  env <- child_env(get_env())

  lazy <- structure(list(expr = quote(foo(bar)), env = env), class = "lazy")
  expect_identical(compat_lazy(lazy), with_env(env, ~foo(bar)))

  lazy_str <- "foo(bar)"
  expect_identical(compat_lazy(lazy_str), ~foo(bar))

  lazy_lang <- quote(foo(bar))
  expect_identical(compat_lazy(lazy_lang), ~foo(bar))

  lazy_sym <- quote(foo)
  expect_identical(compat_lazy(lazy_sym), ~foo)
})

test_that("lazy_dots objects are converted to tidy quotes", {
  env <- child_env(get_env())

  lazy_dots <- structure(class = "lazy_dots", list(
    lazy = structure(list(expr = quote(foo(bar)), env = env), class = "lazy"),
    lazy_lang = quote(foo(bar))
  ))

  expected <- list(
    lazy = with_env(env, ~foo(bar)),
    lazy_lang = ~foo(bar),
    ~foo(bar)
  )

  expect_identical(compat_lazy_dots(lazy_dots, get_env(), "foo(bar)"), expected)
})

test_that("unnamed lazy_dots are given default names", {
  lazy_dots <- structure(class = "lazy_dots", list(
    "foo(baz)",
    quote(foo(bar))
  ))

  expected <- list(
    `foo(baz)` = ~foo(baz),
    `foo(bar)` = ~foo(bar),
    foobarbaz = ~foo(barbaz)
  )
  dots <- compat_lazy_dots(lazy_dots, get_env(), foobarbaz = "foo(barbaz)", .named = TRUE)

  expect_identical(dots, expected)
})
