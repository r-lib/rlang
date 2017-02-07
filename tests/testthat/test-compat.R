context("compat")

test_that("names() dispatches on environment", {
  env <- new_env(data = list(foo = "foo", bar = "bar"))
  expect_identical(sort(names(env)), c("bar", "foo"))
})
