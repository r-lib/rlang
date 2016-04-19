context("complain")

test_that("NULL return unchanged", {
  expect_identical(complain(NULL), NULL)
})

test_that("can't access non-existent list members", {
  x1 <- list(y = 1)
  x2 <- complain(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "object 'z' not found")
  expect_error(x2[["z"]], "object 'z' not found")
})

test_that("can't access non-existent environment components", {
  x1 <- list2env(list(y = 1))
  x2 <- complain(x1)

  expect_equal(x2$y, 1)
  expect_error(x2$z, "object 'z' not found")
  expect_error(x2[["z"]], "object 'z' not found")
})

test_that("can't use non-character vectors", {
  x <- complain(list(y = 1))

  expect_error(x[[1]], "subset with a string")
  expect_error(x[[c("a", "b")]], "subset with a string")
})

test_that("complain doesn't taint env class", {
  x1 <- list2env(list(y = 1))
  x2 <- complain(x1)

  expect_equal(class(x1), "environment")
  expect_equal(class(x2), c("complain", "environment"))

})
