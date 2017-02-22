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

  expect_error(as_symbol(c("a", "b")), "Cannot convert objects of type `character` to `symbol`")
})

test_that("as_call() produces calls", {
  expect_equal(as_call(quote(a)), quote(a()))
  expect_equal(as_call(quote(a())), quote(a()))
  expect_equal(as_call("a()"), quote(a()))
  expect_equal(as_call(~ a()), quote(a()))

  expect_error(as_call(c("a", "b")), "Cannot convert objects of type `character` to `language`")
})

test_that("as_symbol() handles prefixed call names", {
  expect_identical(as_symbol(quote(foo::bar())), quote(foo::bar))
  expect_error(as_symbol(~foo@bar()), "recursive")
})

# misc -------------------------------------------------------------------

test_that("qualified and namespaced symbols are recognised", {
  expect_true(is_qualified_lang(quote(foo@baz())))
  expect_true(is_qualified_lang(quote(foo::bar())))
  expect_false(is_qualified_lang(quote(foo()())))

  expect_false(is_namespaced_lang(quote(foo@bar())))
  expect_true(is_namespaced_lang(quote(foo::bar())))
})
