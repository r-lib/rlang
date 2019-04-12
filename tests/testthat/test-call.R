context("call")

# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(call2(letters), "must be a string")
})

test_that("args can be specified individually or as list", {
  out <- call2("f", a = 1, splice(list(b = 2)))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

test_that("creates namespaced calls", {
  expect_identical(call2("fun", foo = quote(baz), .ns = "bar"), quote(bar::fun(foo = baz)))
})

test_that("fails with non-callable objects", {
  expect_error(call2(1), "non-callable")
  expect_error(call2(current_env()), "non-callable")
})

test_that("succeeds with literal functions", {
  expect_error(regex = NA, call2(base::mean, 1:10))
  expect_error(regex = NA, call2(base::list, 1:10))
})

test_that("call2() preserves empty arguments", {
  expect_identical(call2("[", quote(x), , drop = ), quote(x[, drop = ]))
})

test_that("call2() requires a symbol when namespace is supplied", {
  expect_identical(call2("foo", .ns = "bar"), quote(bar::foo()))
  expect_error(call2(function() NULL, .ns = "bar"), "must be a string or symbol")
  expect_error(call2(quote(foo()), .ns = "bar"), "must be a string or symbol")
})


# Standardisation ---------------------------------------------------------

test_that("call_standardise() supports quosures", {
  fn <- function(foo, bar) "Not this one"

  quo <- local({
    fn <- function(baz, quux) "This one"
    quo(fn(this, that))
  })

  exp <- new_quosure(quote(fn(baz = this, quux = that)), quo_get_env(quo))
  expect_identical(call_standardise(quo), exp)
})

test_that("can standardise primitive functions (#473)", {
  expect_identical(call_standardise(foo ~ bar), foo ~ bar)
  expect_identical(call_standardise(quote(1 + 2)), quote(1 + 2))
})


# Modification ------------------------------------------------------------

test_that("can modify formulas inplace", {
  expect_identical(call_modify(~matrix(bar), quote(foo)), ~matrix(bar, foo))
})

test_that("new args inserted at end", {
  call <- quote(matrix(1:10))
  out <- call_modify(call_standardise(call), nrow = 3)
  expect_equal(out, quote(matrix(data = 1:10, nrow = 3)))
})

test_that("new args replace old", {
  call <- quote(matrix(1:10))
  out <- call_modify(call_standardise(call), data = 3)
  expect_equal(out, quote(matrix(data = 3)))
})

test_that("can modify calls for primitive functions", {
  expect_identical(call_modify(~list(), foo = "bar"), ~list(foo = "bar"))
})

test_that("can modify calls for functions containing dots", {
  expect_identical(call_modify(~mean(), na.rm = TRUE), ~mean(na.rm = TRUE))
})

test_that("accepts unnamed arguments", {
  expect_identical(
    call_modify(~get(), "foo", envir = "bar", "baz"),
    ~get("foo", envir = "bar", "baz")
  )
})

test_that("allows duplicated arguments (#398)", {
  expect_identical(call_modify(~mean(), na.rm = TRUE, na.rm = FALSE), ~mean(na.rm = FALSE))
  expect_identical(call_modify(~mean(), TRUE, FALSE), ~mean(TRUE, FALSE))
  expect_identical(call_modify(~mean(), foo = zap(), foo = zap()), ~mean())
})

test_that("zaps remove arguments", {
  expect_identical(call_modify(quote(foo(bar = )), bar = zap()), quote(foo()))
  expect_identical_(call_modify(quote(foo(bar = , baz = )), !!!rep_named(c("foo", "bar", "baz"), list(zap()))), quote(foo()))
})

test_that("can remove unexisting arguments (#393)", {
  expect_identical(call_modify(quote(foo()), ... = zap()), quote(foo()))
})

test_that("can add a missing argument", {
  expect_identical(call_modify(quote(foo()), bar = expr()), quote(foo(bar = )))
  expect_identical(call_modify(quote(foo()), bar = ), quote(foo(bar = )))
})

test_that("can refer to dots as named argument", {
  expect_error(call_modify(quote(foo()), ... = NULL), "must be `zap\\(\\)` or empty")
  expect_error(call_modify(quote(foo()), ... = "foo"), "must be `zap\\(\\)` or empty")
  expect_identical(call_modify(quote(foo(x, ..., y)), ... = ), quote(foo(x, ..., y)))
  expect_identical(call_modify(quote(foo(x)), ... = ), quote(foo(x, ...)))
  expect_identical(call_modify(quote(foo(x, ..., y)), ... = zap()), quote(foo(x, y)))
})

test_that("can't supply unnamed zaps", {
  expect_error(call_modify(quote(foo(bar)), zap()), "can't be unnamed")
})

test_that("positions are not changed", {
  expect_identical(call_modify(quote(fn(1)), x = "foo"), quote(fn(1, x = "foo")))
  expect_identical(call_modify(quote(fn(x = 1)), x = "foo"), quote(fn(x = "foo")))
  expect_identical(call_modify(quote(fn(1, x = 1)), x = "foo"), quote(fn(1, x = "foo")))
  expect_identical(call_modify(quote(fn(x = 1, 1)), x = "foo"), quote(fn(x = "foo", 1)))

  expect_identical(call_modify(quote(fn(1)), ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn(...)), ... = ), quote(fn(...)))
  expect_identical(call_modify(quote(fn(1, ...)), ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn(..., 1)), ... = ), quote(fn(..., 1)))

  expect_identical(call_modify(quote(fn()), 1, x = "foo"), quote(fn(1, x = "foo")))
  expect_identical(call_modify(quote(fn()), x = 1, x = "foo"), quote(fn(x = "foo")))
  expect_identical(call_modify(quote(fn()), 1, x = 1, x = "foo"), quote(fn(1, x = "foo")))
  expect_identical(call_modify(quote(fn()), x = 1, 1, x = "foo"), quote(fn(x = "foo", 1)))

  expect_identical(call_modify(quote(fn()), 1, ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn()), ... = , ... = ), quote(fn(...)))
  expect_identical(call_modify(quote(fn()), 1, ... = , ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn()), ... = , 1, ... = ), quote(fn(..., 1)))
})

test_that("empty quosures are treated as empty args", {
  expect_identical(call_modify(quote(fn()), ... = quo()), quote(fn(...)))
})


# Utils --------------------------------------------------------------

test_that("NULL is a valid language object", {
  expect_true(is_expression(NULL))
})

test_that("is_call() pattern-matches", {
  expect_true(is_call(quote(foo(bar)), "foo"))
  expect_false(is_call(quote(foo(bar)), "bar"))
  expect_true(is_call(quote(foo(bar)), quote(foo)))

  expect_true(is_call(quote(foo(bar)), "foo", n = 1))
  expect_false(is_call(quote(foo(bar)), "foo", n = 2))
  expect_true(is_call(quote(+3), n = 1))
  expect_true(is_call(quote(3 + 3), n = 2))

  expect_true(is_call(quote(foo::bar())), quote(foo::bar()))

  expect_false(is_call(1))
  expect_false(is_call(NULL))
})

test_that("is_call() vectorises name", {
  expect_false(is_call(quote(foo::bar), c("fn", "fn2")))
  expect_true(is_call(quote(foo::bar), c("fn", "::")))

  expect_true(is_call(quote(foo::bar), quote(`::`)))
  expect_true(is_call(quote(foo::bar), list(quote(`@`), quote(`::`))))
  expect_false(is_call(quote(foo::bar), list(quote(`@`), quote(`:::`))))
})

test_that("call_name() handles namespaced and anonymous calls", {
  expect_equal(call_name(quote(foo::bar())), "bar")
  expect_equal(call_name(quote(foo:::bar())), "bar")

  expect_null(call_name(quote(foo@bar())))
  expect_null(call_name(quote(foo$bar())))
  expect_null(call_name(quote(foo[[bar]]())))
  expect_null(call_name(quote(foo()())))
  expect_null(call_name(quote(foo::bar()())))
  expect_null(call_name(quote((function() NULL)())))
})

test_that("call_name() handles formulas", {
  expect_identical(call_name(~foo(baz)), "foo")
})

test_that("call_fn() extracts function", {
  fn <- function() call_fn(call_frame())
  expect_identical(fn(), fn)

  expect_identical(call_fn(~matrix()), matrix)
})

test_that("call_fn() looks up function in `env`", {
  env <- local({
    fn <- function() "foo"
    current_env()
  })
  expect_identical(call_fn(quote(fn()), env = env), env$fn)
})

test_that("Inlined functions return NULL name", {
  call <- quote(fn())
  call[[1]] <- function() {}
  expect_null(call_name(call))
})

test_that("call_args() and call_args_names()", {
  expect_identical(call_args(~fn(a, b)), set_names(list(quote(a), quote(b)), c("", "")))

  fn <- function(a, b) call_args_names(call_frame())
  expect_identical(fn(a = foo, b = bar), c("a", "b"))
})

test_that("qualified and namespaced symbols are recognised", {
  expect_true(is_qualified_call(quote(foo@baz())))
  expect_true(is_qualified_call(quote(foo::bar())))
  expect_false(is_qualified_call(quote(foo()())))

  expect_false(is_namespaced_call(quote(foo@bar())))
  expect_true(is_namespaced_call(quote(foo::bar())))
})

test_that("can specify ns in namespaced predicate", {
  expr <- quote(foo::bar())
  expect_false(is_namespaced_call(expr, quote(bar)))
  expect_true(is_namespaced_call(expr, quote(foo)))
  expect_true(is_namespaced_call(expr, "foo"))
})

test_that("can specify ns in is_call()", {
  expr <- quote(foo::bar())
  expect_true(is_call(expr, ns = NULL))
  expect_false(is_call(expr, ns = ""))
  expect_false(is_call(expr, ns = "baz"))
  expect_true(is_call(expr, ns = "foo"))
  expect_true(is_call(expr, name = "bar", ns = "foo"))
  expect_false(is_call(expr, name = "baz", ns = "foo"))
})

test_that("can check multiple namespaces with is_call()", {
  expect_true(is_call(quote(foo::quux()), ns = c("foo", "bar")))
  expect_true(is_call(quote(bar::quux()), ns = c("foo", "bar")))
  expect_false(is_call(quote(baz::quux()), ns = c("foo", "bar")))
  expect_false(is_call(quote(quux()), ns = c("foo", "bar")))

  expect_false(is_call(quote(baz::quux()), ns = c("foo", "bar", "")))
  expect_true(is_call(quote(quux()), ns = c("foo", "bar", "")))
})

test_that("can unnamespace calls", {
  expect_identical(call_unnamespace(quote(bar(baz))), quote(bar(baz)))
  expect_identical(call_unnamespace(quote(foo::bar(baz))), quote(bar(baz)))
  expect_identical(call_unnamespace(quote(foo@bar(baz))), quote(foo@bar(baz)))
})

test_that("precedence of regular calls", {
  expect_true(call_has_precedence(quote(1 + 2), quote(foo(1 + 2))))
  expect_true(call_has_precedence(quote(foo()), quote(1 + foo())))
})

test_that("precedence of associative ops", {
  expect_true(call_has_precedence(quote(1 + 2), quote(1 + 2 + 3), "lhs"))
  expect_false(call_has_precedence(quote(2 + 3), quote(1 + 2 + 3), "rhs"))
  expect_false(call_has_precedence(quote(1^2), quote(1^2^3), "lhs"))
  expect_true(call_has_precedence(quote(2^3), quote(1^2^3), "rhs"))
})

test_that("call functions type-check their input (#187)", {
  x <- list(a = 1)
  expect_error(call_modify(x, NULL), "must be a quoted call")
  expect_error(call_standardise(x), "must be a quoted call")
  expect_error(call_fn(x), "must be a quoted call")
  expect_error(call_name(x), "must be a quoted call")
  expect_error(call_args(x), "must be a quoted call")
  expect_error(call_args_names(x), "must be a quoted call")

  q <- quo(!!x)
  expect_error(call_modify(q, NULL), "must be a quoted call")
  expect_error(call_standardise(q), "must be a quoted call")
  expect_error(call_fn(q), "must be a quoted call")
  expect_error(call_name(q), "must be a quoted call")
  expect_error(call_args(q), "must be a quoted call")
  expect_error(call_args_names(q), "must be a quoted call")
})

test_that("call_print_type() returns correct enum", {
  expect_error(call_print_type(""), "must be a call")
  expect_identical(call_print_type(quote(foo())), "prefix")

  expect_identical(call_print_type(quote(~a)), "prefix")
  expect_identical(call_print_type(quote(?a)), "prefix")
  expect_identical_(call_print_type(quote(!b)), "prefix")
  expect_identical_(call_print_type(quote(`!!`(b))), "prefix")
  expect_identical_(call_print_type(quote(`!!!`(b))), "prefix")
  expect_identical(call_print_type(quote(+a)), "prefix")
  expect_identical(call_print_type(quote(-a)), "prefix")

  expect_identical(call_print_type(quote(while (a) b)), "special")
  expect_identical(call_print_type(quote(for (a in b) b)), "special")
  expect_identical(call_print_type(quote(repeat a)), "special")
  expect_identical(call_print_type(quote(if (a) b)), "special")
  expect_identical(call_print_type(quote((a))), "special")
  expect_identical(call_print_type(quote({ a })), "special")
  expect_identical(call_print_type(quote(a[b])), "special")
  expect_identical(call_print_type(quote(a[[b]])), "special")

  expect_identical(call_print_type(quote(a ? b)), "infix")
  expect_identical(call_print_type(quote(a ~ b)), "infix")
  expect_identical(call_print_type(quote(a <- b)), "infix")
  expect_identical(call_print_type(quote(a <<- b)), "infix")
  expect_identical(call_print_type(quote(a < b)), "infix")
  expect_identical(call_print_type(quote(a <= b)), "infix")
  expect_identical(call_print_type(quote(a > b)), "infix")
  expect_identical(call_print_type(quote(a >= b)), "infix")
  expect_identical(call_print_type(quote(`=`(a, b))), "infix")
  expect_identical(call_print_type(quote(a == b)), "infix")
  expect_identical(call_print_type(quote(a:b)), "infix")
  expect_identical(call_print_type(quote(a::b)), "infix")
  expect_identical(call_print_type(quote(a:::b)), "infix")
  expect_identical(call_print_type(quote(a := b)), "infix")
  expect_identical(call_print_type(quote(a | b)), "infix")
  expect_identical(call_print_type(quote(a || b)), "infix")
  expect_identical(call_print_type(quote(a & b)), "infix")
  expect_identical(call_print_type(quote(a && b)), "infix")
  expect_identical(call_print_type(quote(a + b)), "infix")
  expect_identical(call_print_type(quote(a - b)), "infix")
  expect_identical(call_print_type(quote(a * b)), "infix")
  expect_identical(call_print_type(quote(a / b)), "infix")
  expect_identical(call_print_type(quote(a ^ b)), "infix")
  expect_identical(call_print_type(quote(a$b)), "infix")
  expect_identical(call_print_type(quote(a@b)), "infix")
  expect_identical(call_print_type(quote(a %% b)), "infix")
  expect_identical(call_print_type(quote(a %>% b)), "infix")

  expect_identical(call_print_type(quote(`?`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`~`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`<`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`<=`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`>`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`>=`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`==`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`:`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`:=`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`|`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`||`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`&`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`&&`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`+`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`-`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`*`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`/`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`^`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`%%`(a, b, c))), "prefix")
  expect_identical(call_print_type(quote(`%>%`(a, b, c))), "prefix")

  expect_identical(call_print_type(quote(`<-`(a, b, c))), "infix")
  expect_identical(call_print_type(quote(`<<-`(a, b, c))), "infix")
  expect_identical(call_print_type(quote(`=`(a, b, c))), "infix")
  expect_identical(call_print_type(quote(`::`(a, b, c))), "infix")
  expect_identical(call_print_type(quote(`:::`(a, b, c))), "infix")
  expect_identical(call_print_type(quote(`$`(a, b, c))), "infix")
  expect_identical(call_print_type(quote(`@`(a, b, c))), "infix")
})

test_that("call_print_fine_type() returns correct enum", {
  expect_error(call_print_fine_type(""), "must be a call")
  expect_identical(call_print_fine_type(quote(foo())), "call")

  expect_identical(call_print_fine_type(quote(~a)), "prefix")
  expect_identical(call_print_fine_type(quote(?a)), "prefix")
  expect_identical_(call_print_fine_type(quote(!b)), "prefix")
  expect_identical_(call_print_fine_type(quote(`!!`(b))), "prefix")
  expect_identical_(call_print_fine_type(quote(`!!!`(b))), "prefix")
  expect_identical(call_print_fine_type(quote(+a)), "prefix")
  expect_identical(call_print_fine_type(quote(-a)), "prefix")

  expect_identical(call_print_fine_type(quote(while (a) b)), "control")
  expect_identical(call_print_fine_type(quote(for (a in b) b)), "control")
  expect_identical(call_print_fine_type(quote(repeat a)), "control")
  expect_identical(call_print_fine_type(quote(if (a) b)), "control")
  expect_identical(call_print_fine_type(quote((a))), "delim")
  expect_identical(call_print_fine_type(quote({ a })), "delim")
  expect_identical(call_print_fine_type(quote(a[b])), "subset")
  expect_identical(call_print_fine_type(quote(a[[b]])), "subset")

  expect_identical(call_print_fine_type(quote(a ? b)), "infix")
  expect_identical(call_print_fine_type(quote(a ~ b)), "infix")
  expect_identical(call_print_fine_type(quote(a <- b)), "infix")
  expect_identical(call_print_fine_type(quote(a <<- b)), "infix")
  expect_identical(call_print_fine_type(quote(a < b)), "infix")
  expect_identical(call_print_fine_type(quote(a <= b)), "infix")
  expect_identical(call_print_fine_type(quote(a > b)), "infix")
  expect_identical(call_print_fine_type(quote(a >= b)), "infix")
  expect_identical(call_print_fine_type(quote(`=`(a, b))), "infix")
  expect_identical(call_print_fine_type(quote(a == b)), "infix")
  expect_identical(call_print_fine_type(quote(a:b)), "infix")
  expect_identical(call_print_fine_type(quote(a::b)), "infix")
  expect_identical(call_print_fine_type(quote(a:::b)), "infix")
  expect_identical(call_print_fine_type(quote(a := b)), "infix")
  expect_identical(call_print_fine_type(quote(a | b)), "infix")
  expect_identical(call_print_fine_type(quote(a || b)), "infix")
  expect_identical(call_print_fine_type(quote(a & b)), "infix")
  expect_identical(call_print_fine_type(quote(a && b)), "infix")
  expect_identical(call_print_fine_type(quote(a + b)), "infix")
  expect_identical(call_print_fine_type(quote(a - b)), "infix")
  expect_identical(call_print_fine_type(quote(a * b)), "infix")
  expect_identical(call_print_fine_type(quote(a / b)), "infix")
  expect_identical(call_print_fine_type(quote(a ^ b)), "infix")
  expect_identical(call_print_fine_type(quote(a$b)), "infix")
  expect_identical(call_print_fine_type(quote(a@b)), "infix")
  expect_identical(call_print_fine_type(quote(a %% b)), "infix")
  expect_identical(call_print_fine_type(quote(a %>% b)), "infix")

  expect_identical(call_print_fine_type(quote(`?`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`~`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`<`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`<=`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`>`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`>=`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`==`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`:`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`:=`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`|`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`||`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`&`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`&&`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`+`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`-`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`*`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`/`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`^`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`%%`(a, b, c))), "call")
  expect_identical(call_print_fine_type(quote(`%>%`(a, b, c))), "call")

  expect_identical(call_print_fine_type(quote(`<-`(a, b, c))), "infix")
  expect_identical(call_print_fine_type(quote(`<<-`(a, b, c))), "infix")
  expect_identical(call_print_fine_type(quote(`=`(a, b, c))), "infix")
  expect_identical(call_print_fine_type(quote(`::`(a, b, c))), "infix")
  expect_identical(call_print_fine_type(quote(`:::`(a, b, c))), "infix")
  expect_identical(call_print_fine_type(quote(`$`(a, b, c))), "infix")
  expect_identical(call_print_fine_type(quote(`@`(a, b, c))), "infix")
})

test_that("call_name() fails with namespaced objects (#670)", {
  skip("Disabled for the 0.3.1 release")
  expect_error(call_name(~foo::bar), "`call` must be a quoted call")
  expect_error(call_name(~foo:::bar), "`call` must be a quoted call")
})

test_that("call_ns() retrieves namespaces", {
  expect_error(call_ns(quote(foo)), "must be a quoted call")
  expect_null(call_ns(quote(foo())))
  expect_identical(call_ns(quote(foo::bar())), "foo")
  expect_identical(call_ns(quote(foo:::bar())), "foo")
})
