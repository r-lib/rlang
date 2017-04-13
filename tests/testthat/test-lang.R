context("lang")

test_that("NULL is a valid language object", {
  expect_true(is_expr(NULL))
})

test_that("is_lang() pattern-matches", {
  expect_true(is_lang(quote(foo(bar)), "foo"))
  expect_false(is_lang(quote(foo(bar)), "bar"))
  expect_true(is_lang(quote(foo(bar)), quote(foo)))

  expect_true(is_lang(~foo(bar), "foo", n = 1))
  expect_false(is_lang(~foo(bar), "foo", n = 2))

  expect_true(is_lang(~foo::bar()), quote(foo::bar()))

  expect_false(is_lang(~1))
  expect_false(is_lang(~NULL))

  expect_true(is_unary_lang(~ +3))
  expect_true(is_binary_lang(~ 3 + 3))
})

test_that("is_lang() vectorises name", {
  expect_false(is_lang(~foo::bar, c("fn", "fn2")))
  expect_true(is_lang(~foo::bar, c("fn", "::")))

  expect_true(is_lang(~foo::bar, quote(`::`)))
  expect_true(is_lang(~foo::bar, list(quote(`@`), quote(`::`))))
  expect_false(is_lang(~foo::bar, list(quote(`@`), quote(`:::`))))
})


# coercion ----------------------------------------------------------------

test_that("as_symbol() produces names", {
  expect_equal(as_symbol("a"), quote(a))
  expect_equal(as_symbol(quote(a)), quote(a))
  expect_equal(as_symbol(quote(a())), quote(a))
  expect_equal(as_symbol(~ a), quote(a))
  expect_equal(as_symbol(~ a()), quote(a))

  expect_error(as_symbol(c("a", "b")), "Can't convert a character vector to a symbol")
})

test_that("as_lang() produces calls", {
  expect_equal(as_lang(quote(a)), quote(a()))
  expect_equal(as_lang(quote(a())), quote(a()))
  expect_equal(as_lang("a()"), quote(a()))
  expect_equal(as_lang(~ a()), quote(a()))

  expect_error(as_lang(c("a", "b")), "Can't convert a character vector to a call \\(lang\\)")
})

test_that("as_symbol() handles prefixed call names", {
  expect_identical(as_symbol(quote(foo::bar())), quote(foo::bar))
  expect_error(as_symbol(~foo@bar()), "recursive")
})

test_that("as_symbol() handles bad calls", {
  call <- quote(foo())
  mut_node_car(call, base::list)
  expect_error(as_symbol(call), "inlined call")
  expect_error(as_symbol(~foo()()), "recursive call")
})

test_that("as_name() returns symbol as string", {
  expect_identical(as_name(~foo(bar)), "foo")
})


# misc -------------------------------------------------------------------

test_that("qualified and namespaced symbols are recognised", {
  expect_true(is_qualified_lang(quote(foo@baz())))
  expect_true(is_qualified_lang(quote(foo::bar())))
  expect_false(is_qualified_lang(quote(foo()())))

  expect_false(is_namespaced_lang(quote(foo@bar())))
  expect_true(is_namespaced_lang(quote(foo::bar())))
})
