utils::globalVariables(c("setClass", "R6Class", "new_class"))

test_that("obj_type_oo() works", {
  bare <- list()
  expect_equal(obj_type_oo(bare), "bare")

  s3 <- mtcars
  expect_equal(obj_type_oo(s3), "S3")

  import_or_skip("methods", "setClass")
  setClass("s4", "integer", where = environment())
  s4 <- new("s4", 1L)
  expect_equal(obj_type_oo(s4), "S4")

  import_or_skip("R6", "R6Class")
  r6 <- R6Class("r6")$new()
  expect_equal(obj_type_oo(r6), "R6")

  import_or_skip("R7", "new_class")
  r7 <- new_class("r7")()
  expect_equal(obj_type_oo(r7), "R7")
})

test_that("stop_input_type() handles I() in `arg` (#1607)", {
  expect_snapshot({
    err(checker(1, stop_input_type, what = "a logical", arg = I("Element 1 of `x`")))
  })
})
