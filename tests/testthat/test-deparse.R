context("deparse")

test_that("line_push() adds indentation", {
  out <- line_push("foo", "bar", width = 4, indent = 2)
  expect_identical(out, c("foo", "  bar"))
})

test_that("line_push() doesn't make a new line if current is only spaces", {
  expect_identical(line_push("    ", "foo", width = 2L), "    foo")
})

test_that("line_push() trims trailing spaces", {
  expect_identical(line_push("foo  ", "bar", width = 1L), c("foo", "bar"))
})

test_that("line_push() doesn't trim trailing spaces on sticky inputs", {
  expect_identical(line_push("tag", " = ", sticky = TRUE, width = 3L, indent = 2L), "tag = ")
})

test_that("sticky input sticks", {
  expect_identical(line_push("foo  ", "bar", sticky = TRUE, width = 1L), "foo  bar")
})

test_that("line_push() respects boundaries", {
  expect_identical(line_push("foo, ", "bar", boundary = 4L, width = 1L, indent = 2L), c("foo,", "  bar"))
  expect_identical(line_push("foo, ", "bar", sticky = TRUE, boundary = 4L, width = 1L, indent = 2L), c("foo,", "  bar"))
  expect_identical(line_push("foo, bar", "baz", boundary = 4L, width = 1L, indent = 2L), c("foo, bar", "  baz"))
})

test_that("line_push() handles the nchar(line) == boundary case", {
  expect_identical(line_push("  tag = ", "bar", sticky = TRUE, boundary = 8L, width = 3L, indent = 2L), "  tag = bar")
})

test_that("control flow is deparsed", {
  expect_identical(while_deparse(quote(while(1) 2)), "while (1) 2")
  expect_identical(for_deparse(quote(for(a in 2) 3)), "for (a in 2) 3")
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

  expect_identical(sexp_deparse(quote({ 1; 2 } + { 3; 4 })), c("{", "  1", "  2", "} + {", "  3", "  4", "}"))
})

test_that("unspaced operators are deparsed", {
  expect_identical(unspaced_op_deparse(quote(1:2)), "1:2")
  expect_identical(unspaced_op_deparse(quote(1^2)), "1^2")
  expect_identical(unspaced_op_deparse(quote(a$b)), "a$b")
  expect_identical(unspaced_op_deparse(quote(a@b)), "a@b")
  expect_identical(unspaced_op_deparse(quote(a::b)), "a::b")
  expect_identical(unspaced_op_deparse(quote(a:::b)), "a:::b")
})

test_that("operands are wrapped in parentheses to ensure correct predecence", {
  expect_identical_(sexp_deparse(expr(1 + !!quote(2 + 3))), "1 + (2 + 3)")
  expect_identical_(sexp_deparse(expr((!!quote(1^2))^3)), "(1^2)^3")
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
  expect_identical(call_deparse(quote(foo(one = bar, two = baz))), "foo(one = bar, two = baz)")
})

test_that("call_deparse() respects boundaries", {
  ctxt <- new_lines(width = 1L)
  expect_identical(call_deparse(quote(foo(bar, baz)), ctxt), c("foo(", "  bar,", "  baz)"))

  ctxt <- new_lines(width = 7L)
  expect_identical(call_deparse(quote(foo(bar, baz)), ctxt), c("foo(", "  bar,", "  baz)"))

  ctxt <- new_lines(width = 8L)
  expect_identical(call_deparse(quote(foo(bar, baz)), ctxt), c("foo(bar,", "  baz)"))

  ctxt <- new_lines(width = 1L)
  expect_identical(call_deparse(quote(foo(one = bar, two = baz)), ctxt), c("foo(", "  one = bar,", "  two = baz)"))
})

test_that("call_deparse() handles multi-line arguments", {
  ctxt <- new_lines(width = 1L)
  expect_identical(sexp_deparse(quote(foo(one = 1, two = nested(one = 1, two = 2))), ctxt), c("foo(", "  one = 1,", "  two = nested(", "    one = 1,", "    two = 2))"))

  ctxt <- new_lines(width = 20L)
  expect_identical(sexp_deparse(quote(foo(one = 1, two = nested(one = 1, two = 2))), ctxt), c("foo(one = 1, two = nested(", "    one = 1, two = 2))"))
})
