context("attributes")

test_that("names2() takes care of missing values", {
  x <- set_names(1:3, c("a", NA, "b"))
  expect_identical(names2(x), c("a", "", "b"))
})

test_that("names2() fails for environments", {
  expect_error(names2(env()), "Use env_names() for environments.", fixed = TRUE)
})

test_that("set_attrs() fails with uncopyable types", {
  expect_error(set_attrs(env(), foo = "bar"), "is uncopyable")
})

test_that("mut_attrs() fails with copyable types", {
  expect_error(mut_attrs(letters, foo = "bar"), "is copyable")
})

test_that("set_attrs() called with NULL zaps attributes", {
  obj <- set_attrs(letters, foo = "bar")
  expect_identical(set_attrs(obj, NULL), letters)
})

test_that("set_attrs() does not zap old attributes", {
  obj <- set_attrs(letters, foo = "bar")
  obj <- set_attrs(obj, baz = "bam")
  expect_named(attributes(obj), c("foo", "baz"))
})
