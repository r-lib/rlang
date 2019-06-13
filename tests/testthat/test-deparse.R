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

test_that("line_push() strips ANSI codes before computing overflow", {
  if (!has_crayon()) {
    skip("test needs crayon")
  }
  expect_identical(length(line_push("foo", open_blue(), width = 3L)), 2L)
  expect_identical(length(line_push("foo", open_blue(), width = 3L, has_colour = TRUE)), 1L)
})

test_that("can push several lines (useful for default base deparser)", {
  expect_identical(new_lines()$push(c("foo", "bar"))$get_lines(), "foobar")
})

test_that("control flow is deparsed", {
  expect_identical(fn_call_deparse(expr(function(a, b) 1)), "function(a, b) 1")
  expect_identical(fn_call_deparse(expr(function(a = 1, b = 2) { 3; 4; 5 })), c("function(a = 1, b = 2) {", "  3", "  4", "  5", "}"))
  expect_identical(while_deparse(quote(while(1) 2)), "while (1) 2")
  expect_identical(for_deparse(quote(for(a in 2) 3)), "for (a in 2) 3")
  expect_identical(repeat_deparse(quote(repeat 1)), "repeat 1")
  expect_identical(if_deparse(quote(if (1) 2 else { 3 })), c("if (1) 2 else {", "  3", "}"))
})

test_that("functions defs increase indent", {
  ctxt <- new_lines(width = 3L)
  expect_identical(sexp_deparse(quote(function() 1), ctxt), c("function()", "  1"))

  ctxt <- new_lines(width = 3L)
  expect_identical(sexp_deparse(function() 1, ctxt), c("<function()", "  1>"))
})

test_that("blocks are deparsed", {
  expect_identical(braces_deparse(quote({1; 2; { 3; 4 }})), c("{", "  1", "  2", "  {", "    3", "    4", "  }", "}"))
  expect_identical_(sexp_deparse(quote({{ 1 }})), c("{", "  {", "    1", "  }", "}"))

  ctxt <- new_lines(width = 3L)
  expected_lines <- c("{", "  11111", "  22222", "  {", "    33333", "    44444", "  }", "}")
  expect_identical(braces_deparse(quote({11111; 22222; { 33333; 44444 }}), ctxt), expected_lines)
})

test_that("multiple openers on the same line only trigger one indent", {
  ctxt <- new_lines(width = 3L)
  expect_identical(sexp_deparse(quote(function() { 1 }), ctxt), c("function()", "  {", "    1", "  }"))

  ctxt <- new_lines(width = 12L)
  expect_identical(sexp_deparse(quote(function() { 1 }), ctxt), c("function() {", "  1", "}"))
})

test_that("multiple openers on the same line are correctly reset", {
  expect_identical(sexp_deparse(quote({ 1(2()) })), c("{", "  1(2())", "}"))
})

test_that("parentheses are deparsed", {
  expect_identical(parens_deparse(quote((1))), "(1)")
  expect_identical(parens_deparse(quote(({ 1; 2 }))), c("({", "  1", "  2", "})"))
  expect_identical(sexp_deparse(quote(({({ 1 })}))), c("({", "  ({", "    1", "  })", "})"))
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
  expect_identical_(sexp_deparse(quote(function() 1 ? 2)), "function() 1 ? 2")
  expect_identical_(sexp_deparse(expr(!!quote(function() 1) ? 2)), "(function() 1) ? 2")
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

test_that("brackets are deparsed", {
  expect_identical(sexp_deparse(quote(1[2])), c("1[2]"))
  expect_identical(sexp_deparse(quote(1[[2]])), c("1[[2]]"))

  ctxt <- new_lines(width = 1L)
  expect_identical(sexp_deparse(quote(1[2]), ctxt), c("1[", "  2]"))
  ctxt <- new_lines(width = 1L)
  expect_identical(sexp_deparse(quote(1[[2]]), ctxt), c("1[[", "  2]]"))
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
  expect_identical(sexp_deparse(quote(foo(one = 1, two = nested(one = 1, two = 2))), ctxt), c("foo(one = 1, two = nested(", "  one = 1, two = 2))"))
})

test_that("call_deparse() delimits CAR when needed", {
  call <- expr((!!quote(function() x + 1))())
  expect_identical(call_deparse(call), "(function() x + 1)()")

  # Only equal because of the extra parentheses
  expect_equal(parse_expr(expr_deparse(call)), call)

  call <- expr((!!quote(f + g))(x))
  expect_identical(call_deparse(call), "`+`(f, g)(x)")
  expect_identical(parse_expr(expr_deparse(call)), call)

  call <- expr((!!quote(+f))(x))
  expect_identical(call_deparse(call), "`+`(f)(x)")
  expect_identical(parse_expr(expr_deparse(call)), call)

  call <- expr((!!quote(while (TRUE) NULL))(x))
  expect_identical(call_deparse(call), "`while`(TRUE, NULL)(x)")
  expect_identical(parse_expr(expr_deparse(call)), call)

  call <- expr(foo::bar(x))
  expect_identical(call_deparse(call), "foo::bar(x)")
  expect_identical(parse_expr(expr_deparse(call)), call)
})

test_that("literal functions are deparsed", {
  expect_identical_(sexp_deparse(function(a) 1), "<function(a) 1>")
  expect_identical_(sexp_deparse(expr(foo(!!function(a) 1))), "foo(<function(a) 1>)")
})

test_that("literal dots are deparsed", {
  dots <- (function(...) env_get(, "..."))(NULL)
  expect_identical_(sexp_deparse(expr(foo(!!dots))), "foo(<...>)")
})

test_that("environments are deparsed", {
  expect_identical(sexp_deparse(expr(foo(!! env()))), "foo(<environment>)")
})

test_that("atomic vectors are deparsed", {
  expect_identical(sexp_deparse(set_names(c(TRUE, FALSE, TRUE), c("", "b", ""))), "<lgl: TRUE, b = FALSE, TRUE>")
  expect_identical(sexp_deparse(set_names(1:3, c("", "b", ""))), "<int: 1L, b = 2L, 3L>")
  expect_identical(sexp_deparse(set_names(c(1, 2, 3), c("", "b", ""))), "<dbl: 1, b = 2, 3>")
  expect_identical(sexp_deparse(set_names(as.complex(1:3), c("", "b", ""))), "<cpl: 1+0i, b = 2+0i, 3+0i>")
  expect_identical(sexp_deparse(set_names(as.character(1:3), c("", "b", ""))), "<chr: \"1\", b = \"2\", \"3\">")
  expect_identical(sexp_deparse(set_names(as.raw(1:3), c("", "b", ""))), "<raw: 01, b = 02, 03>")
})

test_that("boundaries are respected when deparsing vectors", {
  ctxt <- new_lines(width = 1L)
  vec <- set_names(1:3, c("", "b", ""))
  expect_identical_(sexp_deparse(expr(foo(!!vec)), ctxt), c("foo(", "  <int:", "    1L,", "    b = 2L,", "    3L>)"))

  ctxt <- new_lines(width = 12L)
  expect_identical(sexp_deparse(list(c("foo", "bar", "baz")), ctxt), c("<list: <chr:", "  \"foo\",", "  \"bar\",", "  \"baz\">>"))
})

test_that("scalar atomic vectors are simply printed", {
  expect_identical(sexp_deparse(TRUE), "TRUE")
  expect_identical(sexp_deparse(1L), "1L")
  expect_identical(sexp_deparse(1), "1")
  expect_identical(sexp_deparse(1i), "0+1i")
  expect_identical(sexp_deparse("1"), "\"1\"")
})

test_that("scalar raw vectors are printed in long form", {
  expect_identical(sexp_deparse(as.raw(1)), "<raw: 01>")
})

test_that("literal lists are deparsed", {
  expect_identical(sexp_deparse(list(TRUE, b = 2L, 3, d = "4", as.raw(5))), "<list: TRUE, b = 2L, 3, d = \"4\", <raw: 05>>")
})

test_that("long vectors are truncated", {
  expect_identical(sexp_deparse(1:10), "<int: 1L, 2L, 3L, 4L, 5L, ...>")
  expect_identical(sexp_deparse(as.list(1:10)), "<list: 1L, 2L, 3L, 4L, 5L, ...>")
})

test_that("other objects are deparsed with base deparser", {
  expect_identical_(sexp_deparse(expr(foo((!!base::list)(1, 2)))), "foo(.Primitive(\"list\")(1, 2))")
  expect_identical_(sexp_deparse(expr(foo((!!base::`if`)(1, 2)))), "foo(.Primitive(\"if\")(1, 2))")
})

test_that("S3 objects are deparsed", {
  expr <- expr(list(!!factor(1:3), !!structure(list(), class = c("foo", "bar", "baz"))))
  expect_identical(sexp_deparse(expr), "list(<fct>, <S3: foo>)")
})

test_that("successive indentations on a single line are only counted once", {
  ctxt <- new_lines(5L)
  broken_output <- c("<list:", "  <chr:", "    foo = \"bar\",", "    baz = \"bam\">>")
  expect_identical(sexp_deparse(list(c(foo = "bar", baz = "bam")), ctxt), broken_output)

  ctxt <- new_lines(12L)
  unbroken_output <- c("<list: <chr:", "  foo = \"bar\",", "  baz = \"bam\">>")
  expect_identical(sexp_deparse(list(c(foo = "bar", baz = "bam")), ctxt), unbroken_output)
})

test_that("successive indentations close off properly", {
  expect_identical(sexp_deparse(quote(1(2(), 3(4())))), "1(2(), 3(4()))")
  expect_identical(sexp_deparse(quote(1(2(), 3(4()))), new_lines(width = 1L)), c("1(", "  2(),", "  3(", "    4()))"))
  expect_identical(sexp_deparse(expr(c((1), function() { 2 }))), c("c((1), function() {", "  2", "})"))
})

test_that("empty quosures are deparsed", {
  expect_identical(strip_style(quo_deparse(quo())), "^")
})

test_that("missing values are deparsed", {
  expect_identical(sexp_deparse(NA), "NA")
  expect_identical(sexp_deparse(c(name = NA)), "<lgl: name = NA>")
  expect_identical(sexp_deparse(NA_integer_), "<int: NA>")
  expect_identical(sexp_deparse(NA_real_), "<dbl: NA>")
  expect_identical(sexp_deparse(NA_complex_), "<cpl: NA>")
  expect_identical(sexp_deparse(NA_character_), "<chr: NA>")
  expect_identical(sexp_deparse(c(NA, "NA")), "<chr: NA, \"NA\">")

  expect_identical(sexp_deparse(quote(call(NA))), "call(NA)")
  expect_identical(sexp_deparse(quote(call(NA_integer_))), "call(<int: NA>)")
  expect_identical(sexp_deparse(quote(call(NA_real_))), "call(<dbl: NA>)")
  expect_identical(sexp_deparse(quote(call(NA_complex_))), "call(<cpl: NA>)")
  expect_identical(sexp_deparse(quote(call(NA_character_))), "call(<chr: NA>)")
})

test_that("needs_backticks() detects non-syntactic symbols", {
  expect_true(all(map_lgl(reserved_words, needs_backticks)))

  expect_false(any(map_lgl(c(".", "a", "Z"), needs_backticks)))

  expect_true(all(map_lgl(c("1", ".1", "~", "!"), needs_backticks)))
  expect_true(all(map_lgl(c("_", "_foo", "1foo"), needs_backticks)))
  expect_true(all(map_lgl(c(".fo!o", "b&ar", "baz <- _baz", "~quux.", "h~unoz_"), needs_backticks)))

  expect_false(any(map_lgl(c(".foo", "._1", "bar", "baz_baz", "quux.", "hunoz_", "..."), needs_backticks)))

  expect_false(needs_backticks(expr()))
})

test_that("expr_text() and expr_name() interpret unicode tags (#611)", {
  expect_identical(expr_text(quote(`<U+006F>`)), "o")
  expect_identical(expr_name(quote(`~f<U+006F><U+006F>`)), "~foo")
  expect_identical(as_label(quote(`~f<U+006F><U+006F>`)), "~foo")
})

test_that("expr_text() deparses non-syntactic symbols with backticks (#211)", {
  expect_identical(expr_text(sym("~foo")), "`~foo`")
  expect_identical(expr_text(sym("~f<U+006F><U+006F>")), "`~foo`")
  expect_identical(expr_text(call("~foo")), "`~foo`()")
})

test_that("expr_text() deparses empty arguments", {
  expect_identical(expr_text(expr()), "")
  expect_identical(quo_text(expr()), "")
  expect_identical(quo_text(quo()), "")
})

test_that("expr_name() deparses empty arguments", {
  expect_identical(expr_name(expr()), "")
  expect_identical(quo_name(quo()), "")
  expect_identical(names(quos_auto_name(quos(, ))), "<empty>")
  expect_identical(as_label(expr()), "<empty>")
})

test_that("expr_deparse() handles newlines in strings (#484)", {
  x <- "foo\n"

  expect_identical(expr_deparse(x), "\"foo\\n\"")
  expect_output(expr_print(x), "foo\\n", fixed = TRUE)

  roundtrip <- parse_expr(expr_deparse(x))
  expect_identical(x, roundtrip)
})

test_that("expr_deparse() handles ANSI escapes in strings", {
  expect_identical(expr_deparse("\\"), deparse("\\"))
  expect_identical(expr_deparse("\\a"), deparse("\\a"))
  expect_identical(expr_deparse("\\b"), deparse("\\b"))
  expect_identical(expr_deparse("\\f"), deparse("\\f"))
  expect_identical(expr_deparse("\\n"), deparse("\\n"))
  expect_identical(expr_deparse("\\r"), deparse("\\r"))
  expect_identical(expr_deparse("\\t"), deparse("\\t"))
  expect_identical(expr_deparse("\\v"), deparse("\\v"))
  expect_identical(expr_deparse("\\0"), deparse("\\0"))
})

test_that("as_label() and expr_name() handles .data pronoun", {
  expect_identical(expr_name(quote(.data[["bar"]])), "bar")
  expect_identical(quo_name(quo(.data[["bar"]])), "bar")
  expect_identical(as_label(quote(.data[["bar"]])), "bar")
  expect_identical(as_label(quo(.data[["bar"]])), "bar")
})

test_that("as_label() handles literals", {
  expect_identical(as_label(1:2), "<int>")
  expect_identical(as_label(c(1, 2)), "<dbl>")
  expect_identical(as_label(letters), "<chr>")
  expect_identical(as_label(base::list), "<fn>")
  expect_identical(as_label(base::mean), "<fn>")
})

test_that("as_label() handles objects", {
  expect_identical(as_label(mtcars), "<data.frame>")
  expect_identical(as_label(structure(1, class = "foo")), "<S3: foo>")
})

test_that("bracket deparsing is a form of argument deparsing", {
  expect_identical(expr_deparse(call("[", iris, missing_arg(), drop = FALSE)), "<data.frame>[, drop = FALSE]")
  expect_identical(expr_deparse(quote(foo[bar, , baz()])), "foo[bar, , baz()]")
  expect_identical(expr_deparse(quote(foo[[bar, , baz()]])), "foo[[bar, , baz()]]")
})

test_that("non-syntactic symbols are deparsed with backticks", {
  expect_identical(expr_deparse(quote(`::foo`)), "`::foo`")
  expect_identical(expr_deparse(quote(x(`_foo`))), "x(`_foo`)")
  expect_identical(expr_deparse(quote(x[`::foo`])), "x[`::foo`]")
})

test_that("symbols with unicode are deparsed consistently (#691)", {
  skip_unless_utf8()
  skip_if(getRversion() < "3.2")

  expect_identical(expr_text(sym("\u00e2a")), "\u00e2a")
  expect_identical(expr_deparse(sym("\u00e2a")), "\u00e2a")

  expect_identical(expr_text(sym("a\u00e2")), "a\u00e2")
  expect_identical(expr_deparse(sym("a\u00e2")), "a\u00e2")
})

test_that("formal parameters are backticked if needed", {
  expect_identical(expr_deparse(function(`^`) {}), c("<function(`^`) { }>"))
})

test_that("empty blocks are deparsed on the same line", {
  expect_identical(expr_deparse(quote({ })), "{ }")
})

test_that("top-level S3 objects are deparsed", {
  f <- structure(function() { }, class = "lambda")
  expect_identical(expr_deparse(f), "<S3: lambda>")
})

# This test causes a parsing failure in R CMD check >= 3.6
#
# test_that("binary operators with 0 or 1 arguments are properly deparsed", {
#   expect_identical_(expr_deparse(quote(`/`())), "`/`()")
#   expect_identical(expr_deparse(quote(`/`("foo"))), "`/`(\"foo\")")
#   expect_identical_(expr_deparse(quote(`::`())), "`::`()")
#   expect_identical(expr_deparse(quote(`::`("foo"))), "`::`(\"foo\")")
# })

test_that("as_label() supports symbols, calls, and literals", {
  expect_identical(as_label(quote(foo)), "foo")
  expect_identical(as_label(quote(foo(bar))), "foo(bar)")
  expect_identical(as_label(1L), "1L")
  expect_identical(as_label("foo"), "\"foo\"")
  expect_identical(as_label(function() NULL), "<fn>")
  expect_identical(as_label(expr(function() { a; b })), "function() ...")
  expect_identical(as_label(1:2), "<int>")
  expect_identical(as_label(env()), "<env>")
})

test_that("as_label() supports special objects", {
  expect_match(as_label(quote(foo := bar)), ":=")
  expect_identical(as_label(quo(foo)), "foo")
  expect_identical(as_label(quo(foo(!!quo(bar)))), "foo(bar)")
  expect_identical(as_label(~foo), "~foo")
  expect_identical(as_label(NULL), "NULL")
})

test_that("as_name() supports quosured symbols and strings", {
   expect_identical(as_name(quo(foo)), "foo")
   expect_identical(as_name(quo("foo")), "foo")
   expect_error(as_name(quo(foo())), "Can't convert a call to a string")
 })
