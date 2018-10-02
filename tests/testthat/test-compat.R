context("compat")

test_that("names() dispatches on environment", {
  env <- child_env(NULL, foo = "foo", bar = "bar")
  expect_identical(sort(names(env)), c("bar", "foo"))
})

test_that("lazy objects are converted to tidy quotes", {
  env <- child_env(current_env())

  lazy <- structure(list(expr = quote(foo(bar)), env = env), class = "lazy")
  expect_identical(compat_lazy(lazy), new_quosure(quote(foo(bar)), env))

  lazy_str <- "foo(bar)"
  expect_identical(compat_lazy(lazy_str), quo(foo(bar)))

  lazy_lang <- quote(foo(bar))
  expect_identical(compat_lazy(lazy_lang), quo(foo(bar)))

  lazy_sym <- quote(foo)
  expect_identical(compat_lazy(lazy_sym), quo(foo))
})

test_that("lazy_dots objects are converted to tidy quotes", {
  env <- child_env(current_env())

  lazy_dots <- structure(class = "lazy_dots", list(
    lazy = structure(list(expr = quote(foo(bar)), env = env), class = "lazy"),
    lazy_lang = quote(foo(bar))
  ))

  expected <- list(
    lazy = new_quosure(quote(foo(bar)), env),
    lazy_lang = quo(foo(bar)),
    quo(foo(bar))
  )

  expect_identical(compat_lazy_dots(lazy_dots, current_env(), "foo(bar)"), expected)
})

test_that("unnamed lazy_dots are given default names", {
  lazy_dots <- structure(class = "lazy_dots", list(
    "foo(baz)",
    quote(foo(bar))
  ))

  expected <- list(
    `foo(baz)` = quo(foo(baz)),
    `foo(bar)` = quo(foo(bar)),
    foobarbaz = quo(foo(barbaz))
  )
  dots <- compat_lazy_dots(lazy_dots, current_env(), foobarbaz = "foo(barbaz)", .named = TRUE)

  expect_identical(dots, expected)
})

test_that("compat_lazy() handles missing arguments", {
  expect_identical(compat_lazy(), quo())
})

test_that("compat_lazy_dots() takes lazy objects", {
  lazy <- structure(list(expr = quote(foo), env = empty_env()), class = "lazy")
  expect_identical(compat_lazy_dots(lazy), named_list(new_quosure(quote(foo), empty_env())))
})

test_that("compat_lazy_dots() takes symbolic objects", {
  expect_identical(compat_lazy_dots(quote(foo), empty_env()), named_list(new_quosure(quote(foo), empty_env())))
  expect_identical(compat_lazy_dots(quote(foo(bar)), empty_env()), named_list(new_quosure(quote(foo(bar)), empty_env())))
})

test_that("compat_lazy() demotes character vectors to strings", {
  expect_identical(compat_lazy_dots(NULL, current_env(), c("foo", "bar")), named_list(as_quosure(~foo)))
})

test_that("compat_lazy() handles numeric vectors", {
  expect_identical(compat_lazy_dots(NULL, current_env(), NA_real_), named_list(set_env(quo(NA_real_))))
  expect_warning(expect_identical(compat_lazy_dots(NULL, current_env(), 1:3), named_list(set_env(quo(1L)))), "Truncating vector")
})

test_that("compat_lazy() handles bare formulas", {
  expect_identical(compat_lazy(~foo), quo(foo))
  expect_identical(compat_lazy_dots(~foo), named_list(quo(foo)))
})

test_that("trimws() trims", {
  x <- "  foo.  "
  expect_identical(trimws(x), "foo.")
  expect_identical(trimws(x, "l"), "foo.  ")
  expect_identical(trimws(x, "r"), "  foo.")
})

test_that("map2() sets names", {
  expect_identical(map2(list(foo = NULL, bar = NULL), 1:2, function(...) NULL), list(foo = NULL, bar = NULL))
})

test_that("map2() discards recycled names", {
  expect_identical(map2(list(foo = NULL), 1:3, function(...) NULL), new_list(3))
})
