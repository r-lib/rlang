context("invoke")

test_that("invoke() buries arguments", {
  expect_identical(invoke(call_inspect, 1:2, 3L), quote(.fn(`1`, `2`, `3`)))
  expect_identical(invoke("call_inspect", 1:2, 3L), quote(call_inspect(`1`, `2`, `3`)))
  expect_identical(invoke(call_inspect, 1:2, 3L, .bury = c("foo", "bar")), quote(foo(`bar1`, `bar2`, `bar3`)))
  expect_identical(invoke(call_inspect, 1:2, 3L, .bury = NULL), as.call(list(call_inspect, 1L, 2L, 3L)))
})

test_that("invoke() can be called without arguments", {
  expect_identical(invoke("list"), list())
  expect_identical(invoke(list), list())
})
