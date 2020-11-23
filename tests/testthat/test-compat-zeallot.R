
test_that("LHS must be a list of symbols wrapped in `c()`", {
  expect_error(
    foo %<-% list(1),
    "must be a `c()` call",
    fixed = TRUE
  )

  expect_error(
    c(foo()) %<-% list(1),
    "must refer to symbols"
  )
})

test_that("can assign lists and vectors", {
  c(foo, bar) %<-% list(a = 1, 2)
  expect_equal(list(foo, bar), list(1, 2))

  c(foo, bar) %<-% c(a = 1, 2)
  expect_equal(list(foo, bar), list(1, 2))
})

test_that("unused elements are ignored", {
  c(foo, bar) %<-% as.list(1:10)
  expect_equal(list(foo, bar), list(1, 2))
})

test_that("must supply a long enough list", {
  expect_error(
    c(foo) %<-% list(),
    "must be long enough"
  )
})
