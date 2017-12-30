context("print")

test_that("push_lines() adds indentation", {
  lines <- c("foo", "  foobarbaz", "  barbazbam", "  bazbam")
  expect_identical(push_lines("foo", c("foobarbaz", "barbazbam", "baz", "bam"), width = 8, indent = 2), lines)
})

test_that("push_lines() doesn't make a new line if current is only spaces", {
  expect_identical(push_lines("    ", "foo", width = 2L), "    foo")
})
