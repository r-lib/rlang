context("print")

test_that("push_lines() adds indentation", {
  lines <- c("foo", "  foobarbaz", "  barbazbam", "  bazbam")
  expect_identical(push_lines("foo", c("foobarbaz", "barbazbam", "baz", "bam"), width = 8, indent = 2), lines)
})

test_that("push_lines() doesn't make a new line if current is only spaces", {
  expect_identical(push_lines("    ", "foo", width = 2L), "    foo")
})

test_that("control flow is deparsed", {
  expect_identical(while_deparse(quote(while(1)2(3))), "while (1) 2(3)")
  expect_identical(for_deparse(quote(for(a in 2(3))4)), "for (a in 2(3)) 4")
  expect_identical(if_deparse(quote(if(1)2 else { 3 })), c("if (1) 2 else {", "  3", "}"))
})

test_that("blocks are deparsed", {
  expect_identical(braces_deparse(quote({1; 2; { 3; 4 }})), c("{", "  1", "  2", "  {", "    3", "    4", "  }", "}"))

  ctxt <- new_lines(width = 3L)
  expected_lines <- c("{", "  11111", "  22222", "  {", "    33333", "    44444", "  }", "}")
  expect_identical(braces_deparse(quote({11111; 22222; { 33333; 44444 }}), ctxt), expected_lines)
})
