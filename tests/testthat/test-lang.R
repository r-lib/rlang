context("language")

test_that("NULL is a valid language object", {
  expect_true(is_expr(NULL))
})

test_that("is_call() pattern-matches", {
  expect_true(is_call(quote(foo(bar)), "foo"))
  expect_false(is_call(quote(foo(bar)), "bar"))
  expect_true(is_call(quote(foo(bar)), quote(foo)))

  expect_true(is_call(~foo(bar), "foo", n = 1))
  expect_false(is_call(~foo(bar), "foo", n = 2))

  expect_true(is_call(~foo::bar()), quote(foo::bar()))

  expect_false(is_call(~1))
  expect_false(is_call(~NULL))

  expect_true(is_unary_call(~ +3))
  expect_true(is_binary_call(~ 3 + 3))
})

test_that("is_call() vectorises name", {
  expect_false(is_call(~foo::bar, c("fn", "fn2")))
  expect_true(is_call(~foo::bar, c("fn", "::")))

  expect_true(is_call(~foo::bar, quote(`::`)))
  expect_true(is_call(~foo::bar, list(quote(`@`), quote(`::`))))
  expect_false(is_call(~foo::bar, list(quote(`@`), quote(`:::`))))
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
  expect_identical(as_symbol(~foo@bar()), quote(foo@bar))
})
