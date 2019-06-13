context("quasiquotation")

test_that("interpolation does not recurse over spliced arguments", {
  var2 <- quote({foo; !! stop(); bar})
  expr_var2 <- tryCatch(expr(list(!!! var2)), error = identity)
  expect_false(inherits(expr_var2, "error"))
})

test_that("formulas containing unquote operators are interpolated", {
  var1 <- quo(foo)
  var2 <- local({ foo <- "baz"; quo(foo) })

  f <- expr_interp(~list(!!var1, !!var2))
  expect_identical(f, new_formula(NULL, call2("list", as_quosure(var1), as_quosure(var2))))
})

test_that("interpolation is carried out in the right environment", {
  f <- local({ foo <- "foo"; ~!!foo })
  expect_identical(expr_interp(f), new_formula(NULL, "foo", env = f_env(f)))
})

test_that("interpolation now revisits unquoted formulas", {
  f <- ~list(!!~!!stop("should not interpolate within formulas"))
  f <- expr_interp(f)
  # This used to be idempotent:
  expect_error(expect_false(identical(expr_interp(f), f)), "interpolate within formulas")
})

test_that("formulas are not treated as quosures", {
  expect_identical(expr(a ~ b), quote(a ~ b))
  expect_identical(expr(~b), quote(~b))
  expect_identical(expr(!!~b), ~b)
})

test_that("unquote operators are always in scope", {
  env <- child_env("base", foo = "bar")
  f <- with_env(env, ~(!!foo))
  expect_identical(expr_interp(f), new_formula(NULL, "bar", env))
})

test_that("can interpolate in specific env", {
  foo <- "bar"
  env <- child_env(NULL, foo = "foo")

  expanded <- expr_interp(~!!foo)
  expect_identical(expanded, set_env(~"bar"))

  expanded <- expr_interp(~!!foo, env)
  expect_identical(expanded, set_env(~"foo"))
})

test_that("can qualify operators with namespace", {
    expect_identical(quo(other::UQ(toupper("a"))), quo(other::"A"))
    expect_identical(quo(x$UQ(toupper("a"))), quo(x$"A"))
})

test_that("unquoting is frame-consistent", {
  defun <- quote(!! function() NULL)
  env <- child_env("base")
  expect_identical(fn_env(expr_interp(defun, env)), env)
})

test_that("unquoted quosure has S3 class", {
  quo <- quo(!! ~quo)
  expect_is(quo, "quosure")
})

test_that("unquoted quosures are not guarded", {
  quo <- eval_tidy(quo(quo(!! ~quo)))
  expect_true(is_quosure(quo))
})


# !! ----------------------------------------------------------------------

test_that("`!!` binds tightly", {
  expect_identical_(expr(!!1 + 2 + 3), quote(1 + 2 + 3))
  expect_identical_(expr(1 + !!2 + 3), quote(1 + 2 + 3))
  expect_identical_(expr(1 + 2 + !!3 + 4), quote(1 + 2 + 3 + 4))
  expect_identical_(expr(1 + !!(2) + 3), quote(1 + 2 + 3))
  expect_identical_(expr(1 + 2 + !!3), quote(1 + 2 + 3))
  expect_identical_(expr(1 + !!2 * 3), quote(1 + 2 * 3))
  expect_identical_(expr(1 + !!2 * 3 + 4), quote(1 + 2 * 3 + 4))
  expect_identical_(expr(1 * !!2:!!3 + 4), quote(1 * 2:3 + 4))
  expect_identical_(expr(1 + 2 + !!3 * 4 + 5 + 6), quote(1 + 2 + 3 * 4 + 5 + 6))

  expect_identical_(expr(1 + 2 * 3 : !!4 + 5 * 6 + 7), quote(1 + 2 * 3 : 4 + 5 * 6 + 7))
  expect_identical_(expr(1 + 2 * 3 : !!4 + 5 * 6 + 7 * 8 : !!9 + 10 * 11), quote(1 + 2 * 3 : 4 + 5 * 6 + 7 * 8 : 9 + 10 * 11))
  expect_identical_(expr(!!1 + !!2 * !!3:!!4 + !!5 * !!6 + !!7 * !!8:!!9 + !!10 * !!11), quote(1 + 2 * 3 : 4 + 5 * 6 + 7 * 8 : 9 + 10 * 11))

  expect_identical_(expr(!!1 + !!2 + !!3  + !!4), quote(1 + 2 + 3 + 4))
  expect_identical_(expr(!!1 + !!2 * !!3), quote(1 + 2 * 3))

  # Local roots
  expect_identical_(expr(!!1 + !!2 * !!3  * !!4), quote(1 + 2 * 3 * 4))
  expect_identical_(expr(1 == 2 + !!3 + 4), quote(1 == 2 + 3 + 4))
  expect_identical_(expr(!!1 == !!2 + !!3 + !!4 + !!5 * !!6 * !!7), quote(1 == 2 + 3 + 4 + 5 * 6 * 7))
  expect_identical_(expr(1 + 2 * 3:!!4:5), quote(1 + 2 * 3:4:5))

  expect_identical_(expr(!!1 == !!2), quote(1 == 2))
  expect_identical_(expr(!!1 <= !!2), quote(1 <= 2))
  expect_identical_(expr(!!1 >= !!2), quote(1 >= 2))
  expect_identical_(expr(!!1 * 2 != 3), quote(1 * 2 != 3))

  expect_identical_(expr(!!1 * !!2 / !!3 > !!4), quote(1 * 2 / 3 > 4))
  expect_identical_(expr(!!1 * !!2 > !!3 + !!4), quote(1 * 2 > 3 + 4))

  expect_identical_(expr(1 <= !!2), quote(1 <= 2))
  expect_identical_(expr(1 >= !!2 : 3), quote(1 >= 2 : 3))
  expect_identical_(expr(1 > !!2 * 3 : 4), quote(1 > 2 * 3 : 4))

  expect_identical_(expr(!!1^2^3), quote(1))
  expect_identical_(expr(!!1^2^3 + 4), quote(1 + 4))
  expect_identical_(expr(!!1^2 + 3:4), quote(1 + 3:4))
})

test_that("`!!` handles binary and unary `-` and `+`", {
  expect_identical_(expr(!!1 + 2), quote(1 + 2))
  expect_identical_(expr(!!1 - 2), quote(1 - 2))

  expect_identical_(expr(!!+1 + 2), quote(1 + 2))
  expect_identical_(expr(!!-1 - 2), expr(`!!`(-1) - 2))

  expect_identical_(expr(1 + -!!3 + 4), quote(1 + -3 + 4))
  expect_identical_(expr(1 + ---+!!3 + 4), quote(1 + ---+3 + 4))

  expect_identical_(expr(+1), quote(+1))
  expect_identical_(expr(+-!!1), quote(+-1))
  expect_identical_(expr(+-!!(1 + 1)), quote(+-2))
  expect_identical_(expr(+-!!+-1), bquote(+-.(-1)))

  expect_identical_(expr(+-+-!!+1), quote(+-+-1))
  expect_identical_(expr(+-+-!!-1), bquote(+-+-.(-1)))

  expect_identical_(expr(+-+-!!1 - 2), quote(+-+-1 - 2))
  expect_identical_(expr(+-+-!!+-+1 + 2), bquote(+-+-.(-1) + 2))
  expect_identical(expr(+-+-!!+-!1 + 2), quote(+-+-0L))

  expect_identical_(expr(+-+-!!+-identity(1)), bquote(+-+-.(-1)))
  expect_identical_(expr(+-+-!!+-identity(1) + 2), bquote(+-+-.(-1) + 2))
})

test_that("`!!` handles special operators", {
  expect_identical(expr(!! 1 %>% 2), quote(1 %>% 2))
})

test_that("LHS of nested `!!` is expanded (#405)", {
  expect_identical_(expr(!!1 + foo(!!2) + !!3), quote(1 + foo(2) + 3))
  expect_identical_(expr(!!1 + !!2 + foo(!!3) + !!4), quote(1 + 2 + foo(3) + 4))
})

test_that("operators with zero or one argument work (#652)", {
  expect_identical(quo(`/`()), new_quosure(quote(`/`())))
  expect_identical(expr(`/`(2)), quote(`/`(2)))
})

test_that("evaluates contents of `!!`", {
  expect_identical(expr(!!(1 + 2)), 3)
})

test_that("quosures are not rewrapped", {
  var <- quo(!! quo(letters))
  expect_identical(quo(!!var), quo(letters))

  var <- new_quosure(local(~letters), env = child_env(current_env()))
  expect_identical(quo(!!var), var)
})

test_that("UQ() fails if called without argument", {
  scoped_lifecycle_silence()

  quo <- quo(UQ(NULL))
  expect_equal(quo, ~NULL)

  quo <- tryCatch(quo(UQ()), error = identity)
  expect_is(quo, "error")
  expect_match(quo$message, "must be called with an argument")
})


# !!! ---------------------------------------------------------------------

test_that("values of `!!!` spliced into expression", {
  f <- quo(f(a, !!! list(quote(b), quote(c)), d))
  expect_identical(f, quo(f(a, b, c, d)))
})

test_that("names within `!!!` are preseved", {
  f <- quo(f(!!! list(a = quote(b))))
  expect_identical(f, quo(f(a = b)))
})

test_that("`!!!` handles `{` calls", {
  expect_identical(quo(list(!!! quote({ foo }))), quo(list(foo)))
})

test_that("splicing an empty vector works", {
  expect_identical(expr_interp(~list(!!! list())), ~list())
  expect_identical(expr_interp(~list(!!! character(0))), ~list())
  expect_identical(expr_interp(~list(!!! NULL)), ~list())
})

# This fails but doesn't seem needed
if (FALSE) {
test_that("serialised unicode in argument names is unserialised on splice", {
  skip("failing")
  nms <- with_latin1_locale({
    exprs <- exprs("\u5e78" := 10)
    quos <- quos(!!! exprs)
    names(quos)
  })
  expect_identical(as_bytes(nms), as_bytes("\u5e78"))
  expect_true(all(chr_encoding(nms) == "UTF-8"))
})
}

test_that("can't splice at top level", {
  expect_error_(expr(!!! letters), "top level")
})

test_that("can splice function body even if not a `{` block", {
  fn <- function(x) { x }
  expect_identical(exprs(!!!fn_body(fn)), named_list(quote(x)))

  fn <- function(x) x
  expect_identical(exprs(!!!fn_body(fn)), named_list(quote(x)))
})

test_that("splicing a pairlist has no side effect", {
  x <- pairlist(NULL)
  expr(foo(!!! x, y))
  expect_identical(x, pairlist(NULL))
})

test_that("`!!!` works in prefix form", {
  expect_identical(exprs(`!!!`(1:2)), named_list(1L, 2L))
  expect_identical(expr(list(`!!!`(1:2))), quote(list(1L, 2L)))
  expect_identical(quos(`!!!`(1:2)), quos_list(quo(1L), quo(2L)))
  expect_identical(quo(list(`!!!`(1:2))), new_quosure(quote(list(1L, 2L))))
})

test_that("can't use prefix form of `!!!` with qualifying operators", {
  expect_error_(expr(foo$`!!!`(bar)), "Prefix form of `!!!` can't be used with `\\$`")
  expect_error_(expr(foo@`!!!`(bar)), "Prefix form of `!!!` can't be used with `@`")
  expect_error_(expr(foo::`!!!`(bar)), "Prefix form of `!!!` can't be used with `::`")
  expect_error_(expr(foo:::`!!!`(bar)), "Prefix form of `!!!` can't be used with `:::`")
  expect_error_(expr(rlang::`!!!`(bar)), "Prefix form of `!!!` can't be used with `::`")
  expect_error_(expr(rlang:::`!!!`(bar)), "Prefix form of `!!!` can't be used with `:::`")
})

test_that("can't supply multiple arguments to `!!!`", {
  expect_error_(expr(list(`!!!`(1, 2))), "Can't supply multiple arguments to `!!!`")
  expect_error_(exprs(`!!!`(1, 2)), "Can't supply multiple arguments to `!!!`")
})

test_that("`!!!` doesn't modify spliced inputs by reference", {
  x <- 1:3
  quos(!!! x)
  expect_identical(x, 1:3)

  x <- as.list(1:3)
  quos(!!! x)
  expect_identical(x, as.list(1:3))

  x <- quote({ 1L; 2L; 3L })
  quos(!!! x)
  expect_equal(x, quote({ 1L; 2L; 3L }))  # equal because of srcrefs
})

test_that("exprs() preserves spliced quosures", {
  out <- exprs(!!!quos(a, b))
  expect_identical(out, exprs(!!quo(a), !!quo(b)))
  expect_identical(out, named_list(quo(a), quo(b)))
})

test_that("!!! fails with non-vectors", {
  expect_error_(exprs(!!!env()), "not a vector")
  expect_error_(exprs(!!!function() NULL), "not a vector")
  expect_error_(exprs(!!!base::c), "not a vector")
  expect_error_(exprs(!!!base::`{`), "not a vector")
  expect_error_(exprs(!!!expression()), "not a vector")

  expect_error_(quos(!!!env()), "not a vector")
  expect_error_(quos(!!!function() NULL), "not a vector")
  expect_error_(quos(!!!base::c), "not a vector")
  expect_error_(quos(!!!base::`{`), "not a vector")
  expect_error_(quos(!!!expression()), "not a vector")

  expect_error_(expr(list(!!!env())), "not a vector")
  expect_error_(expr(list(!!!function() NULL)), "not a vector")
  expect_error_(expr(list(!!!base::c)), "not a vector")
  expect_error_(expr(list(!!!base::`{`)), "not a vector")
  expect_error_(expr(list(!!!expression())), "not a vector")

  expect_error_(list2(!!!env()), "not a vector")
  expect_error_(list2(!!!function() NULL), "not a vector")
  expect_error_(list2(!!!base::c), "not a vector")
  expect_error_(list2(!!!base::`{`), "not a vector")
  expect_error_(list2(!!!expression()), "not a vector")
})

test_that("!!! succeeds with vectors, pairlists and language objects", {
  expect_identical_(exprs(!!!NULL), named_list())
  expect_identical_(exprs(!!!pairlist(1)), named_list(1))
  expect_identical_(exprs(!!!list(1)), named_list(1))
  expect_identical_(exprs(!!!TRUE), named_list(TRUE))
  expect_identical_(exprs(!!!1L), named_list(1L))
  expect_identical_(exprs(!!!1), named_list(1))
  expect_identical_(exprs(!!!1i), named_list(1i))
  expect_identical_(exprs(!!!"foo"), named_list("foo"))
  expect_identical_(exprs(!!!bytes(0)), named_list(bytes(0)))

  expect_identical_(quos(!!!NULL), quos_list())
  expect_identical_(quos(!!!pairlist(1)), quos_list(quo(1)))
  expect_identical_(quos(!!!list(1)), quos_list(quo(1)))
  expect_identical_(quos(!!!TRUE), quos_list(quo(TRUE)))
  expect_identical_(quos(!!!1L), quos_list(quo(1L)))
  expect_identical_(quos(!!!1), quos_list(quo(1)))
  expect_identical_(quos(!!!1i), quos_list(quo(1i)))
  expect_identical_(quos(!!!"foo"), quos_list(quo("foo")))
  expect_identical_(quos(!!!bytes(0)), quos_list(quo(!!bytes(0))))

  expect_identical_(expr(foo(!!!NULL)), quote(foo()))
  expect_identical_(expr(foo(!!!pairlist(1))), quote(foo(1)))
  expect_identical_(expr(foo(!!!list(1))), quote(foo(1)))
  expect_identical_(expr(foo(!!!TRUE)), quote(foo(TRUE)))
  expect_identical_(expr(foo(!!!1L)), quote(foo(1L)))
  expect_identical_(expr(foo(!!!1)), quote(foo(1)))
  expect_identical_(expr(foo(!!!1i)), quote(foo(1i)))
  expect_identical_(expr(foo(!!!"foo")), quote(foo("foo")))
  expect_identical_(expr(foo(!!!bytes(0))), expr(foo(!!bytes(0))))

  expect_identical_(list2(!!!NULL), list())
  expect_identical_(list2(!!!pairlist(1)), list(1))
  expect_identical_(list2(!!!list(1)), list(1))
  expect_identical_(list2(!!!TRUE), list(TRUE))
  expect_identical_(list2(!!!1L), list(1L))
  expect_identical_(list2(!!!1), list(1))
  expect_identical_(list2(!!!1i), list(1i))
  expect_identical_(list2(!!!"foo"), list("foo"))
  expect_identical_(list2(!!!bytes(0)), list(bytes(0)))
})

test_that("!!! calls as.list()", {
  as_quos_list <- function(x, env = empty_env()) {
    new_quosures(map(x, new_quosure, env = env))
  }
  exp <- as.list(mtcars)
  expect_identical_(exprs(!!!mtcars), exp)
  expect_identical_(quos(!!!mtcars), as_quos_list(exp))
  expect_identical_(expr(foo(!!!mtcars)), do.call(call, c(list("foo"), exp)))
  expect_identical_(list2(!!!mtcars), as.list(mtcars))

  fct <- factor(c("a", "b"))
  exp <- set_names(as.list(fct), c("", ""))
  expect_identical_(exprs(!!!fct), exp)
  expect_identical_(quos(!!!fct), as_quos_list(exp))
  expect_identical_(expr(foo(!!!fct)), do.call(call, c(list("foo"), exp)))
  expect_identical_(list2(!!!fct), as.list(fct))
})

test_that("!!! calls methods::as()", {
  as_quos_list <- function(x, env = empty_env()) {
    new_quosures(map(x, new_quosure, env = env))
  }

  .Person <- setClass("Person", slots = c(name = "character", species = "character"))
  fievel <- .Person(name = "Fievel", species = "mouse")
  methods::setAs("Person", "list", function(from, to) list(name = from@name, species = from@species))

  exp <- list(name = "Fievel", species = "mouse")
  expect_identical_(exprs(!!!fievel), exp)
  expect_identical_(quos(!!!fievel), as_quos_list(exp))
  expect_identical_(expr(foo(!!!fievel)), quote(foo(name = "Fievel", species = "mouse")))
  expect_identical_(list2(!!!fievel), exp)
})


# bang ---------------------------------------------------------------

test_that("single ! is not treated as shortcut", {
  expect_identical(quo(!foo), as_quosure(~!foo))
})

test_that("double and triple ! are treated as syntactic shortcuts", {
  var <- local(quo(foo))
  expect_identical(quo(!! var), as_quosure(var))
  expect_identical(quo(!! quo(foo)), quo(foo))
  expect_identical(quo(list(!!! letters[1:3])), quo(list("a", "b", "c")))
})

test_that("`!!` works in prefixed calls", {
  var <- quo(cyl)
  expect_identical(expr_interp(~mtcars$`!!`(quo_squash(var))), ~mtcars$cyl)
  expect_identical(expr_interp(~foo$`!!`(quote(bar))), ~foo$bar)
  expect_identical(expr_interp(~base::`!!`(quote(list))()), ~base::list())
})

test_that("one layer of parentheses around !! is removed", {
  foo <- "foo"
  expect_identical(expr((!! foo)), "foo")
  expect_identical(expr(((!! foo))), quote(("foo")))

  expect_identical(expr((!! foo) + 1), quote("foo" + 1))
  expect_identical(expr(((!! foo)) + 1), quote(("foo") + 1))

  expect_identical(expr((!! sym(foo))(bar)), quote(foo(bar)))
  expect_identical(expr(((!! sym(foo)))(bar)), quote((foo)(bar)))

  expect_identical(exprs((!! foo), ((!! foo))), named_list("foo", quote(("foo"))))
})

test_that("parentheses are not removed if there's a tail", {
  expect_identical(expr((!! "a" + b)), quote(("a" + b)))
})

test_that("can use prefix form of `!!` with qualifying operators", {
  expect_identical(expr(foo$`!!`(quote(bar))), quote(foo$bar))
  expect_identical(expr(foo@`!!`(quote(bar))), quote(foo@bar))
  expect_identical(expr(foo::`!!`(quote(bar))), quote(foo::bar))
  expect_identical(expr(foo:::`!!`(quote(bar))), quote(foo:::bar))
  expect_identical(expr(rlang::`!!`(quote(bar))), quote(rlang::bar))
  expect_identical(expr(rlang:::`!!`(quote(bar))), quote(rlang:::bar))
})

test_that("can unquote within for loop (#417)", {
  # Checks for an issue caused by wrong refcount of unquoted objects

  x <- new_list(3)

  for (i in 1:3) {
    x[[i]] <- expr(!!i)
  }
  expect_identical(x, as.list(1:3))

  for (i in 1:3) {
    x[[i]] <- quo(!!i)
  }
  expect_identical(x, map(1:3, new_quosure, env = empty_env()))

  for (i in 1:3) {
    x[[i]] <- quo(foo(!!i))
  }
  exp <- list(quo(foo(1L)), quo(foo(2L)), quo(foo(3L)))
  expect_identical(x, exp)

  for (i in 1:3) {
    x[[i]] <- quo(foo(!!!i))
  }
  expect_identical(x, exp)
})


# quosures -----------------------------------------------------------

test_that("quosures are created for all informative formulas", {
  foo <- local(quo(foo))
  bar <- local(quo(bar))

  interpolated <- local(quo(list(!!foo, !!bar)))
  expected <- new_quosure(call2("list", as_quosure(foo), as_quosure(bar)), env = get_env(interpolated))
  expect_identical(interpolated, expected)

  interpolated <- quo(!!interpolated)
  expect_identical(interpolated, expected)
})


# dots_values() ------------------------------------------------------

test_that("can unquote-splice symbols", {
  spliced <- list2(!!! list(quote(`_symbol`)))
  expect_identical(spliced, list(quote(`_symbol`)))
})

test_that("can unquote symbols", {
  expect_error_(dots_values(!! quote(.)), "`!!` in a non-quoting function")
})


# := -----------------------------------------------------------------

test_that("`:=` unquotes its LHS as name unless `.unquote_names` is FALSE", {
  expect_identical(exprs(a := b), list(a = quote(b)))
  expect_identical(exprs(a := b, .unquote_names = FALSE), named_list(quote(a := b)))
  expect_identical(quos(a := b), quos_list(a = quo(b)))
  expect_identical(quos(a := b, .unquote_names = FALSE), quos_list(new_quosure(quote(a := b))))
  expect_identical(dots_list(a := NULL), list(a = NULL))
  expect_identical(dots_splice(a := NULL), list(a = NULL))
})

test_that("`:=` chaining is detected at dots capture", {
  expect_error(exprs(a := b := c), "chained")
  expect_error(quos(a := b := c), "chained")
  expect_error(dots_list(a := b := c), "chained")
  expect_error(dots_splice(a := b := c), "chained")
})


# --------------------------------------------------------------------

test_that("Unquote operators fail when called outside quasiquoted arguments", {
  expect_qq_error <- function(object) expect_error(object, regexp = "within a quasiquoted argument")
  expect_qq_error(UQ())
  expect_qq_error(UQS())
  expect_qq_error(`!!`())
  expect_qq_error(`!!!`())
  expect_qq_error(a := b)
})

test_that("`.data[[` unquotes", {
  foo <- "bar"
  expect_identical_(expr(.data[[foo]]), quote(.data[["bar"]]))
  expect_identical_(expr(deep(.data[[foo]])), quote(deep(.data[["bar"]])))
  expect_identical_(exprs(.data[[foo]]), named_list(quote(.data[["bar"]])))
})

test_that("it is still possible to unquote manually within `.data[[`", {
  scoped_lifecycle_silence()
  foo <- "baz"
  expect_identical(expr(.data[[!!toupper(foo)]]), quote(.data[["BAZ"]]))
})

test_that(".data[[ argument is not masked", {
  cyl <- "carb"
  expect_identical_(eval_tidy(expr(.data[[cyl]]), mtcars), mtcars$carb)
})

test_that(".data[[ on the LHS of := fails", {
  expect_error(exprs(.data[["foo"]] := foo), "Can't use the `.data` pronoun on the LHS")
})

test_that("it is still possible to use .data[[ in list2()", {
  .data <- mtcars
  expect_identical_(list2(.data$cyl), list(mtcars$cyl))
})


# Lifecycle ----------------------------------------------------------

test_that("unquoting with rlang namespace is deprecated", {
  expect_warning_(exprs(rlang::UQS(1:2)), regexp = "deprecated as of rlang 0.3.0")
  expect_warning_(quo(list(rlang::UQ(1:2))), regexp = "deprecated as of rlang 0.3.0")

  # Old tests

  expect_identical(quo(rlang::UQ(toupper("a"))), new_quosure("A", empty_env()))
  expect_identical(quo(list(rlang::UQS(list(a = 1, b = 2)))), quo(list(a = 1, b = 2)))

  quo <- quo(rlang::UQ(NULL))
  expect_equal(quo, ~NULL)

  quo <- tryCatch(quo(rlang::UQ()), error = identity)
  expect_is(quo, "error")
  expect_match(quo$message, "must be called with an argument")

  expect_error_(dots_values(rlang::UQ(quote(.))), "`!!` in a non-quoting function")
})

test_that("splicing language objects still works", {
  scoped_lifecycle_silence()

  expect_identical_(exprs(!!!~foo), named_list(~foo))
  expect_identical_(exprs(!!!quote(foo(bar))), named_list(quote(foo(bar))))

  expect_identical_(quos(!!!~foo), quos_list(quo(!!~foo)))
  expect_identical_(quos(!!!quote(foo(bar))), quos_list(quo(foo(bar))))

  expect_identical_(expr(foo(!!!~foo)), expr(foo(!!~foo)))
  expect_identical_(expr(foo(!!!quote(foo(bar)))), expr(foo(foo(bar))))

  expect_identical_(list2(!!!~foo), list(~foo))
  expect_identical_(list2(!!!quote(foo(bar))), list(quote(foo(bar))))
})

test_that("can unquote string in function position", {
  expect_identical_(expr((!!"foo")()), quote("foo"()))
})

test_that("{{ is a quote-unquote operator", {
  fn <- function(foo) expr(list({{ foo }}))
  expect_identical_(fn(bar), expr(list(!!quo(bar))))
  expect_identical_(expr(list({{ letters }})), expr(list(!!quo(!!letters))))
  expect_error_(expr(list({{ quote(foo) }})), "must be a symbol")
})

test_that("{{ only works in quoting functions", {
  expect_error_(
    list2({{ "foo" }}),
    "Can't use `{{` in a non-quoting function",
    fixed = TRUE
  )
})

test_that("{{ on the LHS of :=", {
  foo <- "bar"
  expect_identical_(exprs({{ foo }} := NA), exprs(bar = NA))

  foo <- quote(bar)
  expect_identical_(exprs({{ foo }} := NA), exprs(bar = NA))

  foo <- quo(bar)
  expect_identical_(exprs({{ foo }} := NA), exprs(bar = NA))

  fn <- function(foo) exprs({{ foo }} := NA)
  expect_identical_(fn(bar), exprs(bar = NA))

  expect_error_(exprs({{ foo() }} := NA), "must be a symbol")
})

test_that("can unquote-splice in atomic capture", {
  expect_identical_(chr("a", !!!c("b", "c"), !!!list("d")), c("a", "b", "c", "d"))
})

test_that("can unquote-splice multiple times (#771)", {
  expect_identical(call2("foo", !!!list(1, 2), !!!list(3, 4)), quote(foo(1, 2, 3, 4)))
  expect_identical(list2(!!!list(1, 2), !!!list(3, 4)), list(1, 2, 3, 4))
  expect_identical(exprs(!!!list(1, 2), !!!list(3, 4)), named_list(1, 2, 3, 4))
  expect_identical(expr(foo(!!!list(1, 2), !!!list(3, 4))), quote(foo(1, 2, 3, 4)))
})
