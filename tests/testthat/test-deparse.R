context("deparse")

test_that("push_lines() adds indentation", {
  lines <- c("foo", "  foobarbaz", "  barbazbam", "  bazbam")
  expect_identical(push_lines("foo", c("foobarbaz", "barbazbam", "baz", "bam"), width = 8, indent = 2), lines)
})

test_that("push_lines() doesn't make a new line if current is only spaces", {
  expect_identical(push_lines("    ", "foo", width = 2L), "    foo")
})

test_that("push_lazy_line() stages elements", {
  ctxt <- new_lines(width = 3L)

  ctxt$push("foo")
  ctxt$push_lazy_line("bar")$push_lazy_line("baz")
  expect_identical(ctxt$lines, "foo")

  ctxt$flush()
  expect_identical(ctxt$lines, c("foo", "barbaz"))

  ctxt$push_lazy_line("truc")$push_lazy_line("muche")
  expect_identical(ctxt$lines, c("foo", "barbaz"))

  ctxt$push("bam")
  expect_identical(ctxt$lines, c("foo", "barbaz", "trucmuche", "bam"))
})

test_that("make_last_line_lazy() works", {
  ctxt <- new_lines(width = 3L)
  ctxt$make_last_line_lazy()
  expect_identical(ctxt$lines, chr())
  expect_identical(ctxt$lazy_line, chr())

  ctxt$push("foo")
  ctxt$make_last_line_lazy()
  expect_identical(ctxt$lines, chr())
  expect_identical(ctxt$lazy_line, "foo")

  ctxt$push_lazy_line("bar")$flush()
  expect_identical(ctxt$lines, c("foobar"))
})

test_that("control flow is deparsed", {
  expect_identical(while_deparse(quote(while(1)2(3))), "while (1) 2(3)")
  expect_identical(for_deparse(quote(for(a in 2(3))4)), "for (a in 2(3)) 4")
  expect_identical(repeat_deparse(quote(repeat 1)), "repeat 1")
  expect_identical(if_deparse(quote(if (1) 2 else { 3 })), c("if (1) 2 else {", "  3", "}"))
})

test_that("blocks are deparsed", {
  expect_identical(braces_deparse(quote({1; 2; { 3; 4 }})), c("{", "  1", "  2", "  {", "    3", "    4", "  }", "}"))

  ctxt <- new_lines(width = 3L)
  expected_lines <- c("{", "  11111", "  22222", "  {", "    33333", "    44444", "  }", "}")
  expect_identical(braces_deparse(quote({11111; 22222; { 33333; 44444 }}), ctxt), expected_lines)
})

test_that("parentheses are deparsed", {
  expect_identical(parens_deparse(quote((1))), "(1)")
  expect_identical(parens_deparse(quote(({ 1; 2 }))), c("({", "  1", "  2", "})"))
})

test_that("spaced operators are deparsed", {
  expect_identical(spaced_op_deparse(quote(1 ? 2)), "1 ? 2")
  expect_identical(spaced_op_deparse(quote(1 <- 2)), "1 <- 2")
  expect_identical(spaced_op_deparse(quote(1 <<- 2)), "1 <<- 2")
  expect_identical(spaced_op_deparse(quote(`=`(1, 2))), "1 = 2")
  expect_identical(spaced_op_deparse(quote(1 := 2)), "1 := 2")
  expect_identical(spaced_op_deparse(quote(1 ~ 2)), "1 ~ 2")
  expect_identical(spaced_op_deparse(quote(1 | 2)), "1 | 2")
  expect_identical(spaced_op_deparse(quote(1 || 2)), "1 || 2")
  expect_identical(spaced_op_deparse(quote(1 & 2)), "1 & 2")
  expect_identical(spaced_op_deparse(quote(1 && 2)), "1 && 2")
  expect_identical(spaced_op_deparse(quote(1 > 2)), "1 > 2")
  expect_identical(spaced_op_deparse(quote(1 >= 2)), "1 >= 2")
  expect_identical(spaced_op_deparse(quote(1 < 2)), "1 < 2")
  expect_identical(spaced_op_deparse(quote(1 <= 2)), "1 <= 2")
  expect_identical(spaced_op_deparse(quote(1 == 2)), "1 == 2")
  expect_identical(spaced_op_deparse(quote(1 != 2)), "1 != 2")
  expect_identical(spaced_op_deparse(quote(1 + 2)), "1 + 2")
  expect_identical(spaced_op_deparse(quote(1 - 2)), "1 - 2")
  expect_identical(spaced_op_deparse(quote(1 * 2)), "1 * 2")
  expect_identical(spaced_op_deparse(quote(1 / 2)), "1 / 2")
  expect_identical(spaced_op_deparse(quote(1 %% 2)), "1 %% 2")
  expect_identical(spaced_op_deparse(quote(1 %>% 2)), "1 %>% 2")

  expect_identical(expr_deparse(quote({ 1; 2 } + { 3; 4 })), c("{", "  1", "  2", "} + {", "  3", "  4", "}"))
})

test_that("unspaced operators are deparsed", {
  expect_identical(unspaced_op_deparse(quote(1:2)), "1:2")
  expect_identical(unspaced_op_deparse(quote(1^2)), "1^2")
  expect_identical(unspaced_op_deparse(quote(a$b)), "a$b")
  expect_identical(unspaced_op_deparse(quote(a@b)), "a@b")
  expect_identical(unspaced_op_deparse(quote(a::b)), "a::b")
  expect_identical(unspaced_op_deparse(quote(a:::b)), "a:::b")
})

test_that("unary operators are deparsed", {
  expect_identical(unary_op_deparse(quote(?1)), "?1")
  expect_identical(unary_op_deparse(quote(~1)), "~1")
  expect_identical(unary_op_deparse(quote(!1)), "!1")
  expect_identical_(unary_op_deparse(quote(!!1)), "!!1")
  expect_identical_(unary_op_deparse(quote(!!!1)), "!!!1")
  expect_identical_(unary_op_deparse(quote(`!!`(1))), "!!1")
  expect_identical_(unary_op_deparse(quote(`!!!`(1))), "!!!1")
  expect_identical(unary_op_deparse(quote(+1)), "+1")
  expect_identical(unary_op_deparse(quote(-1)), "-1")
})

test_that("calls are deparsed", {
  expect_identical(call_deparse(quote(foo(bar, baz))), "foo(bar, baz)")
})
