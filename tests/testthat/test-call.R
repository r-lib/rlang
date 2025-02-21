# Creation ----------------------------------------------------------------

test_that("character vector must be length 1", {
  expect_error(call2(letters), "must be a string")
})

test_that("args can be specified individually or as list", {
  out <- call2("f", a = 1, splice(list(b = 2)))
  expect_equal(out, quote(f(a = 1, b = 2)))
})

test_that("creates namespaced calls", {
  expect_identical(
    call2("fun", foo = quote(baz), .ns = "bar"),
    quote(bar::fun(foo = baz))
  )
})

test_that("fails with non-callable objects", {
  expect_error(call2(1), "non-callable")
  expect_error(call2(current_env()), "non-callable")
})

test_that("succeeds with literal functions", {
  expect_error(regexp = NA, call2(base::mean, 1:10))
  expect_error(regexp = NA, call2(base::list, 1:10))
})

test_that("call2() preserves empty arguments", {
  expect_identical(call2("[", quote(x), , drop = ), quote(x[, drop = ]))
})

test_that("call2() requires a symbol when namespace is supplied", {
  expect_identical(call2("foo", .ns = "bar"), quote(bar::foo()))
  expect_error(
    call2(function() NULL, .ns = "bar"),
    "must be a string or symbol"
  )
  expect_error(call2(quote(foo()), .ns = "bar"), "must be a string or symbol")
})


# Standardisation ---------------------------------------------------------

test_that("call_standardise() supports quosures", {
  local_lifecycle_silence()

  fn <- function(foo, bar) "Not this one"

  quo <- local({
    fn <- function(baz, quux) "This one"
    quo(fn(this, that))
  })

  exp <- new_quosure(quote(fn(baz = this, quux = that)), quo_get_env(quo))
  expect_identical(call_standardise(quo), exp)
})

test_that("can standardise primitive functions (#473)", {
  local_lifecycle_silence()

  expect_identical(call_standardise(foo ~ bar), foo ~ bar)
  expect_identical(call_standardise(quote(1 + 2)), quote(1 + 2))
})

test_that("if `call` is supplied to `call_match()`, `fn` must be supplied", {
  expect_error(
    call_match(quote(list())),
    "`fn` must be supplied."
  )
})

test_that("call_match() infers call and definition", {
  fn <- function(foo) call_match(defaults = TRUE)
  expect_equal(fn(), quote(fn(foo = )))
  expect_equal(fn(TRUE), quote(fn(foo = TRUE)))

  # Finds dots
  dots <- function(...) fn(...)
  expect_equal(dots(), quote(fn(foo = )))
  expect_equal(dots(bar), quote(fn(foo = ..1)))
})

test_that("call_match() returns early with primitive functions", {
  expect_equal(
    call_match(quote(x[[1]]), `[[`),
    quote(x[[1]])
  )
})

test_that("call_match() matches defaults", {
  fn <- function(a, b = TRUE, ..., c = FALSE, d) NULL

  expect_equal(
    call_match(quote(fn()), fn, defaults = TRUE),
    quote(fn(a = , b = TRUE, c = FALSE, d = ))
  )
  expect_equal(
    call_match(quote(fn()), fn, defaults = FALSE),
    quote(fn())
  )

  expect_equal(
    call_match(quote(fn(NULL)), fn, defaults = TRUE),
    quote(fn(a = NULL, b = TRUE, c = FALSE, d = ))
  )
  expect_equal(
    call_match(quote(fn(NULL)), fn, defaults = FALSE),
    quote(fn(a = NULL))
  )

  expect_equal(
    call_match(quote(fn(NULL, foo = TRUE)), fn, defaults = TRUE),
    quote(fn(a = NULL, b = TRUE, foo = TRUE, c = FALSE, d = ))
  )
  expect_equal(
    call_match(quote(fn(NULL, foo = TRUE)), fn, defaults = FALSE),
    quote(fn(a = NULL, foo = TRUE))
  )

  expect_equal(
    call_match(
      quote(fn(NULL, foo = TRUE)),
      fn,
      dots_expand = FALSE,
      defaults = TRUE
    ),
    expr(fn(a = NULL, b = TRUE, ... = !!pairlist(foo = TRUE), c = FALSE, d = ))
  )

  expect_equal(
    call_match(
      quote(fn(NULL, foo = TRUE)),
      fn,
      dots_expand = FALSE,
      defaults = FALSE
    ),
    quote(fn(a = NULL, ... = !!pairlist(foo = TRUE)))
  )
})

test_that("`call_match(dots_expand = TRUE)` handles `...` positional edge cases", {
  m <- function(fn) call_match(quote(fn(foo = TRUE)), fn, defaults = FALSE)

  expect_equal(m(function(...) NULL), quote(fn(foo = TRUE)))
  expect_equal(m(function(foo, ...) NULL), quote(fn(foo = TRUE)))
  expect_equal(m(function(..., foo) NULL), quote(fn(foo = TRUE)))
  expect_equal(m(function(foo, ..., bar) NULL), quote(fn(foo = TRUE)))
})


# Modification ------------------------------------------------------------

test_that("can modify formulas inplace", {
  expect_identical(call_modify(~ matrix(bar), quote(foo)), ~ matrix(bar, foo))
})

test_that("new args inserted at end", {
  local_lifecycle_silence()

  call <- quote(matrix(1:10))
  out <- call_modify(call_standardise(call), nrow = 3)
  expect_equal(out, quote(matrix(data = 1:10, nrow = 3)))
})

test_that("new args replace old", {
  local_lifecycle_silence()

  call <- quote(matrix(1:10))
  out <- call_modify(call_standardise(call), data = 3)
  expect_equal(out, quote(matrix(data = 3)))
})

test_that("can modify calls for primitive functions", {
  expect_identical(call_modify(~ list(), foo = "bar"), ~ list(foo = "bar"))
})

test_that("can modify calls for functions containing dots", {
  expect_identical(call_modify(~ mean(), na.rm = TRUE), ~ mean(na.rm = TRUE))
})

test_that("accepts unnamed arguments", {
  expect_identical(
    call_modify(~ get(), "foo", envir = "bar", "baz"),
    ~ get("foo", envir = "bar", "baz")
  )
})

test_that("allows duplicated arguments (#398)", {
  expect_identical(
    call_modify(~ mean(), na.rm = TRUE, na.rm = FALSE),
    ~ mean(na.rm = FALSE)
  )
  expect_identical(call_modify(~ mean(), TRUE, FALSE), ~ mean(TRUE, FALSE))
  expect_identical(call_modify(~ mean(), foo = zap(), foo = zap()), ~ mean())
})

test_that("zaps remove arguments", {
  expect_identical(call_modify(quote(foo(bar = )), bar = zap()), quote(foo()))
  expect_identical_(
    call_modify(
      quote(foo(bar = , baz = )),
      !!!rep_named(c("foo", "bar", "baz"), list(zap()))
    ),
    quote(foo())
  )
})

test_that("can remove unexisting arguments (#393)", {
  expect_identical(call_modify(quote(foo()), ... = zap()), quote(foo()))
})

test_that("can add a missing argument", {
  expect_identical(call_modify(quote(foo()), bar = expr()), quote(foo(bar = )))
  expect_identical(call_modify(quote(foo()), bar = ), quote(foo(bar = )))
})

test_that("can refer to dots as named argument", {
  expect_error(
    call_modify(quote(foo()), ... = NULL),
    "must be `zap\\(\\)` or empty"
  )
  expect_error(
    call_modify(quote(foo()), ... = "foo"),
    "must be `zap\\(\\)` or empty"
  )
  expect_identical(
    call_modify(quote(foo(x, ..., y)), ... = ),
    quote(foo(x, ..., y))
  )
  expect_identical(call_modify(quote(foo(x)), ... = ), quote(foo(x, ...)))
  expect_identical(
    call_modify(quote(foo(x, ..., y)), ... = zap()),
    quote(foo(x, y))
  )
})

test_that("can't supply unnamed zaps", {
  expect_error(call_modify(quote(foo(bar)), zap()), "can't be unnamed")
})

test_that("positions are not changed", {
  expect_identical(
    call_modify(quote(fn(1)), x = "foo"),
    quote(fn(1, x = "foo"))
  )
  expect_identical(
    call_modify(quote(fn(x = 1)), x = "foo"),
    quote(fn(x = "foo"))
  )
  expect_identical(
    call_modify(quote(fn(1, x = 1)), x = "foo"),
    quote(fn(1, x = "foo"))
  )
  expect_identical(
    call_modify(quote(fn(x = 1, 1)), x = "foo"),
    quote(fn(x = "foo", 1))
  )

  expect_identical(call_modify(quote(fn(1)), ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn(...)), ... = ), quote(fn(...)))
  expect_identical(call_modify(quote(fn(1, ...)), ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn(..., 1)), ... = ), quote(fn(..., 1)))

  expect_identical(
    call_modify(quote(fn()), 1, x = "foo"),
    quote(fn(1, x = "foo"))
  )
  expect_identical(
    call_modify(quote(fn()), x = 1, x = "foo"),
    quote(fn(x = "foo"))
  )
  expect_identical(
    call_modify(quote(fn()), 1, x = 1, x = "foo"),
    quote(fn(1, x = "foo"))
  )
  expect_identical(
    call_modify(quote(fn()), x = 1, 1, x = "foo"),
    quote(fn(x = "foo", 1))
  )

  expect_identical(call_modify(quote(fn()), 1, ... = ), quote(fn(1, ...)))
  expect_identical(call_modify(quote(fn()), ... = , ... = ), quote(fn(...)))
  expect_identical(
    call_modify(quote(fn()), 1, ... = , ... = ),
    quote(fn(1, ...))
  )
  expect_identical(
    call_modify(quote(fn()), ... = , 1, ... = ),
    quote(fn(..., 1))
  )
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

test_that("quosures are not calls", {
  skip("Disabled")
  expect_false(is_call(quo()))
})

test_that("is_call() supports symbol `name`", {
  expect_true(is_call(quote(foo()), quote(foo)))
  expect_false(is_call(quote(foo()), quote(bar)))
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
  expect_identical(call_name(~ foo(baz)), "foo")
})

test_that("Inlined functions return NULL name", {
  call <- quote(fn())
  call[[1]] <- function() {
  }
  expect_null(call_name(call))
})

test_that("call_args() and call_args_names() work", {
  expect_equal(
    call_args(~ fn(a, b)),
    set_names(list(quote(a), quote(b)), c("", ""))
  )
  expect_equal(call_args_names(quote(foo(a = , b = ))), c("a", "b"))
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
  expect_snapshot({
    x <- list(a = 1)
    err(call_modify(x, NULL))
    err(call_name(x))
    err(call_args(x))
    err(call_args_names(x))

    q <- quo(!!x)
    err(call_modify(q, NULL))
    err(call_name(q))
    err(call_args(q))
    err(call_args_names(q))
  })
})

test_that("call_print_type() returns correct enum", {
  expect_error(call_print_type(""), "must be a defused call")
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
  expect_identical(
    call_print_type(quote({
      a
    })),
    "special"
  )
  expect_identical(call_print_type(quote(a[b])), "special")
  expect_identical(call_print_type(quote(a[[b]])), "special")

  expect_identical(call_print_type(quote(a?b)), "infix")
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
  expect_identical(call_print_type(quote(a^b)), "infix")
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
  expect_error(call_print_fine_type(""), "must be a defused call")
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
  expect_identical(
    call_print_fine_type(quote({
      a
    })),
    "delim"
  )
  expect_identical(call_print_fine_type(quote(a[b])), "subset")
  expect_identical(call_print_fine_type(quote(a[[b]])), "subset")

  expect_identical(call_print_fine_type(quote(a?b)), "infix")
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
  expect_identical(call_print_fine_type(quote(a^b)), "infix")
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
  expect_true(TRUE)
  return("Disabled for the 0.3.1 release")
  expect_error(call_name(~ foo::bar), "`call` must be a quoted call")
  expect_error(call_name(~ foo:::bar), "`call` must be a quoted call")
})

test_that("call_ns() retrieves namespaces", {
  expect_error(call_ns(quote(foo)), "must be a defused call")
  expect_null(call_ns(quote(foo())))
  expect_identical(call_ns(quote(foo::bar())), "foo")
  expect_identical(call_ns(quote(foo:::bar())), "foo")
})

test_that("is_call_infix() detects infix operators", {
  expect_true(is_call_infix(quote(a %>_>% b)))
  expect_true(is_call_infix(quote(a + b)))
  expect_false(is_call_infix(quote(+b)))
})

test_that("call_zap_inline() works", {
  expect_equal(
    call_zap_inline(quote(foo(1:2))),
    quote(foo(1:2))
  )
  expect_equal(
    call_zap_inline(expr(foo(!!(1:2)))),
    quote(foo(`<int>`))
  )

  expect_equal(
    call_zap_inline(quote(function() 1)),
    quote(function() 1)
  )

  call <- expr(function(x = NULL) foo(!!(1:2)))
  call[[2]]$x <- 1:2
  expect_equal(
    call_zap_inline(call),
    quote(function(x = `<int>`) foo(`<int>`))
  )

  call2 <- expr(function(x = NULL) foo(!!(1:2)))
  call2[[2]]$x <- 1:2

  # No mutation
  expect_equal(call, call2)
})

test_that("is_call_simple() works", {
  expect_false(is_call_simple(quote(foo)))
  expect_false(is_call_simple(quote(foo()())))
  expect_false(is_call_simple(quote(foo::bar)))

  expect_true(is_call_simple(quote(foo())))
  expect_true(is_call_simple(quote(bar::foo())))

  expect_true(is_call_simple(quote(foo()), ns = FALSE))
  expect_false(is_call_simple(quote(foo()), ns = TRUE))
  expect_true(is_call_simple(quote(bar::foo()), ns = TRUE))
  expect_false(is_call_simple(quote(bar::foo()), ns = FALSE))

  expect_true(is_call_simple(~ bar::foo(), ns = TRUE))

  expect_false(is_call_simple(quo()))
})

test_that("call_name() and call_ns() detect `::` calls (#670)", {
  expect_null(call_name(quote(foo::bar)))
  expect_null(call_name(quote(foo:::bar)))
  expect_null(call_ns(quote(foo::bar)))
  expect_null(call_ns(quote(foo:::bar)))
})

test_that("is_call_index() works", {
  expect_true(is_call_index(quote(a$b(...))))
  expect_true(is_call_index(quote(a@b$c[[d]](...))))
  expect_true(is_call_index(quote(a@b$c[[d]](...))))
  expect_true(is_call_index(quote(foo::a$b(...))))

  expect_false(is_call_index(quote(a@b$c[[d]])))
  expect_false(is_call_index(quote(1 + a@b$c[[d]])))
  expect_false(is_call_index(quote((a@b$c[[d]])())))
})

test_that("call_match() supports `...` in arg list when `dots_expand = FALSE`", {
  f <- function(x, ...) NULL
  expect_equal(
    call_match(quote(f(...)), f, dots_expand = FALSE),
    quote(f())
  )
})
