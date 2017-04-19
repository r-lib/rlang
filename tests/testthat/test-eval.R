context("invoke")

test_that("invoke() buries arguments", {
  expect_identical(invoke(call_inspect, 1:2), quote(.fn(`1`, `2`)))
  expect_identical(invoke("call_inspect", 1:2), quote(call_inspect(`1`, `2`)))
  expect_identical(invoke(call_inspect, 1:2, .bury = c("foo", "bar")), quote(foo(`bar1`, `bar2`)))
  expect_identical(invoke(call_inspect, 1:2, .bury = NULL), as.call(list(call_inspect, 1L, 2L)))
})

test_that("invoke() is calls without arguments", {
  expect_identical(invoke("list"), list())
  expect_identical(invoke(list), list())
})
