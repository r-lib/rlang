test_that("can take the address of the missing arg (#1521)", {
  fn <- function(x) obj_address(x)
  expect_true(is_string(fn()))
  expect_true(is_string(obj_address(missing_arg())))
})
