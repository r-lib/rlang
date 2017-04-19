context("lang")

test_that("NULL is a valid language object", {
  expect_true(is_expr(NULL))
})

test_that("is_lang() pattern-matches", {
  expect_true(is_lang(quote(foo(bar)), "foo"))
  expect_false(is_lang(quote(foo(bar)), "bar"))
  expect_true(is_lang(quote(foo(bar)), quote(foo)))

  expect_true(is_lang(quote(foo(bar)), "foo", n = 1))
  expect_false(is_lang(quote(foo(bar)), "foo", n = 2))

  expect_true(is_lang(quote(foo::bar())), quote(foo::bar()))

  expect_false(is_lang(1))
  expect_false(is_lang(NULL))

  expect_true(is_unary_lang(quote(+3)))
  expect_true(is_binary_lang(quote(3 + 3)))
})

test_that("is_lang() vectorises name", {
  expect_false(is_lang(quote(foo::bar), c("fn", "fn2")))
  expect_true(is_lang(quote(foo::bar), c("fn", "::")))

  expect_true(is_lang(quote(foo::bar), quote(`::`)))
  expect_true(is_lang(quote(foo::bar), list(quote(`@`), quote(`::`))))
  expect_false(is_lang(quote(foo::bar), list(quote(`@`), quote(`:::`))))
})


# misc -------------------------------------------------------------------

test_that("qualified and namespaced symbols are recognised", {
  expect_true(is_qualified_lang(quote(foo@baz())))
  expect_true(is_qualified_lang(quote(foo::bar())))
  expect_false(is_qualified_lang(quote(foo()())))

  expect_false(is_namespaced_lang(quote(foo@bar())))
  expect_true(is_namespaced_lang(quote(foo::bar())))
})

test_that("can specify ns in namespaced predicate", {
  expr <- quote(foo::bar())
  expect_false(is_namespaced_lang(expr, quote(bar)))
  expect_true(is_namespaced_lang(expr, quote(foo)))
  expect_true(is_namespaced_lang(expr, "foo"))
})

test_that("can specify ns in is_lang()", {
  expr <- quote(foo::bar())
  expect_true(is_lang(expr, ns = NULL))
  expect_false(is_lang(expr, ns = ""))
  expect_false(is_lang(expr, ns = "baz"))
  expect_true(is_lang(expr, ns = "foo"))
  expect_true(is_lang(expr, name = "bar", ns = "foo"))
  expect_false(is_lang(expr, name = "baz", ns = "foo"))
})

test_that("can unnamespace calls", {
  expect_identical(lang_unnamespace(quote(bar(baz))), quote(bar(baz)))
  expect_identical(lang_unnamespace(quote(foo::bar(baz))), quote(bar(baz)))
  expect_identical(lang_unnamespace(quote(foo@bar(baz))), quote(foo@bar(baz)))
})
