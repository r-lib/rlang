context("eval-tidy")

test_that("accepts expressions", {
  expect_identical(eval_tidy(10), 10)
  expect_identical(eval_tidy(quote(letters)), letters)
})

test_that("eval_tidy uses quosure environment", {
  x <- 10
  quo <- local({
    y <- 100
    quo(x + y)
  })
  expect_equal(eval_tidy(quo), 110)
})

test_that("data must be uniquely named", {
  expect_error(eval_tidy(NULL, list(x = 1, x = 2)), "has duplicate columns")

  data <- set_names(data.frame(x = 1, x = 2, y = 3, y = 4), c("x", "x", "y", "y"))
  expect_error(eval_tidy(NULL, data), "has duplicate columns")
})

test_that("can supply unnamed empty data", {
  expect_identical(eval_tidy("foo", list()), "foo")
  expect_identical(eval_tidy("foo", data.frame()), "foo")
})

test_that("looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(eval_tidy(quo(x), data), 100)
})

test_that("pronouns resolve ambiguity looks first in `data`", {
  x <- 10
  data <- list(x = 100)
  expect_equal(eval_tidy(quo(.data$x), data), 100)
  expect_equal(eval_tidy(quo(.env$x), data), 10)
})

test_that("pronouns complain about missing values", {
  expect_data_pronoun_error(eval_tidy(quo(.data$x), list()), "Column `x` not found in `.data`")
  expect_data_pronoun_error(eval_tidy(quo(.data$x), data.frame()), "Column `x` not found in `.data`")
})

test_that("nested quosures look in their own env", {
  n <- 10
  f <- function() {
    n <- 100
    quo(n)
  }
  quo <- quo(!!f())
  expect_equal(eval_tidy(quo), 100)
})

test_that("nested quosure thunks rechain properly in the non-data mask", {
  bar <- "foo"
  quo <- quo(identity(!!quo(toupper(!!quo(identity(bar))))))
  expect_identical(eval_tidy(quo), "FOO")
})

test_that("unquoted formulas can use data", {
  f1 <- function() {
    z <- 100
    x <- 2
    quo(x + z)
  }
  f2 <- function() {
    z <- 100
    quo(.data$x + .env$z)
  }

  z <- 10
  expect_identical(eval_tidy(f2(), list(x = 1)), 101)
  expect_identical(eval_tidy(quo(!! f1()), data = list(x = 1)), 101)
  expect_identical(eval_tidy(quo(!! f2()), data = list(x = 1)), 101)
})

test_that("bare formulas are not evaluated", {
  f <- local(~x)
  expect_identical(eval_tidy(quo(!! f)), f)

  f <- a ~ b
  expect_identical(eval_tidy(quo(!! f)), f)
})

test_that("quosures are not evaluated if not forced", {
  fn <- function(arg, force) {
    if (force) arg else "bar"
  }

  f1 <- quo(fn(!! quo(stop("forced!")), force = FALSE))
  f2 <- quo(fn(!! local(quo(stop("forced!"))), force = FALSE))
  expect_identical(eval_tidy(f1), "bar")
  expect_identical(eval_tidy(f2), "bar")

  f_forced1 <- quo(fn(!! quo(stop("forced!")), force = TRUE))
  f_forced2 <- quo(fn(!! local(quo(stop("forced!"))), force = TRUE))
  expect_error(eval_tidy(f_forced1), "forced!")
  expect_error(eval_tidy(f_forced2), "forced!")
})

test_that("can unquote captured arguments", {
  var <- quo(cyl)
  fn <- function(arg) eval_tidy(enquo(arg), mtcars)
  expect_identical(fn(var), quo(cyl))
  expect_identical(fn(!!var), mtcars$cyl)
})

test_that("quosures are evaluated recursively", {
  foo <- "bar"
  expect_identical(eval_tidy(quo(foo)), "bar")
  expect_identical(eval_tidy(quo(!!quo(!! quo(foo)))), "bar")
})

test_that("quosures have lazy semantics", {
  fn <- function(arg) "unforced"
  expect_identical(eval_tidy(quo(fn(~stop()))), "unforced")
})

test_that("can unquote hygienically within captured arg", {
  fn <- function(df, arg) eval_tidy(enquo(arg), df)

  foo <- "bar"; var <- quo(foo)
  expect_identical(fn(mtcars, list(var, !!var)), list(quo(foo), "bar"))

  var <- quo(cyl)
  expect_identical(fn(mtcars, (!!var) > 4), mtcars$cyl > 4)
  expect_identical(fn(mtcars, list(var, !!var)), list(quo(cyl), mtcars$cyl))
  expect_equal(fn(mtcars, list(~var, !!var)), list(~var, mtcars$cyl))
  expect_equal(fn(mtcars, list(~~var, !!quo(var), !!quo(quo(var)))), list(~~var, quo(cyl), quo(var)))
})

test_that("can unquote for old-style NSE functions", {
  var <- quo(foo)
  fn <- function(x) substitute(x)
  expect_identical(quo(fn(!!quo_get_expr(var))), quo(fn(foo)))
  expect_identical(eval_tidy(quo(fn(!!quo_get_expr(var)))), quote(foo))
})

test_that("all quosures in the call are evaluated", {
  foobar <- function(x) paste0("foo", x)
  x <- new_quosure(call("foobar", local({ bar <- "bar"; quo(bar) })))
  f <- new_quosure(call("identity", x))
  expect_identical(eval_tidy(f), "foobar")
})

test_that("two-sided formulas are not treated as quosures", {
  expect_identical(eval_tidy(new_quosure(a ~ b)), a ~ b)
})

test_that("formulas are evaluated in evaluation environment", {
  f <- eval_tidy(quo(foo ~ bar), list(foo = "bar"))
  expect_false(identical(f_env(f), current_env()))
})

test_that("evaluation env is cleaned up", {
  f <- local(quo(function() list(f = ~letters, env = environment())))
  fn <- eval_tidy(f)
  out <- fn()
  expect_identical(out$f, with_env(env = out$env, ~letters))
})

test_that("inner formulas are rechained to evaluation env", {
  env <- child_env(NULL)
  f1 <- quo(env$eval_env1 <- current_env())
  f2 <- quo({
    !! f1
    env$eval_env2 <- current_env()
  })

  eval_tidy(f2, mtcars)
  expect_identical(env$eval_env1, env$eval_env2)
  expect_true(env_inherits(env$eval_env2, get_env(f2)))
})

test_that("empty quosure self-evaluates", {
  quo <- quo(is_missing(!! quo()))
  expect_true(eval_tidy(quo))
})

test_that("cannot replace elements of pronouns", {
  expect_error(eval_tidy(quo(.data$foo <- "bar"), mtcars), "Can't modify the data pronoun")
})

test_that("formulas are not evaluated as quosures", {
  expect_identical(eval_tidy(~letters), ~letters)
})

test_that("tilde calls are evaluated in overscope", {
  quo <- quo({
    foo <- "foo"
    ~foo
  })
  f <- eval_tidy(quo)
  expect_true(env_has(f, "foo"))
})

test_that(".env pronoun refers to current quosure (#174)", {
  inner_quo <- local({
    var <- "inner"
    quo(.env$var)
  })

  outer_quo <- local({
    var <- "outer"
    quo(identity(!! inner_quo))
  })

  expect_identical(eval_tidy(outer_quo, list()), "inner")
})

test_that("can call tilde with named arguments (#226)", {
  expect_equal(eval_tidy(quote(`~`(foo = x, bar = y))), x ~ y)
  expect_equal(eval_tidy(quote(`~`(foo = x, bar = y, baz = z))), `~`(foo = x, bar = y, baz = z))
})

test_that("Arguments to formulas are not stripped from their attributes (#227)", {
  quo <- quo(x)

  f <- eval_tidy(quo(~!!quo))
  expect_identical(f_rhs(f), quo)

  f <- eval_tidy(quo(!!quo(x) ~ a))
  expect_identical(f_lhs(f), quo)
})

test_that("evaluating an empty quosure fails", {
  expect_error(eval_tidy(quo()), "not found")
})

test_that("can supply a data mask as data", {
  mask <- as_data_mask(list(x = 1L))
  eval_tidy(quo(x <- 2L), mask)
  expect_identical(eval_tidy(quo(x), mask), 2L)
})

test_that("as_data_pronoun() creates pronoun", {
  data <- as_data_pronoun(mtcars)
  expect_is(data, "rlang_data_pronoun")

  data_env <- .subset2(data, 1)
  expect_reference(env_parent(data_env), empty_env())
  expect_true(all(env_names(data_env) %in% names(mtcars)))

  expect_data_pronoun_error(data$foobar, "Column `foobar` not found in `.data`")
  expect_identical(data[["cyl"]], mtcars$cyl)
})

test_that("can create pronoun from a mask", {
  top <- env(a = 1)
  bottom <- env(top, b = 2)
  mask <- new_data_mask(bottom, top)

  .data <- as_data_pronoun(mask)
  expect_is(.data, "rlang_data_pronoun")
  expect_identical(.data$a, 1)
  expect_identical(.data$b, 2)
})

test_that("pronoun has print() and str() method", {
  data <- as_data_pronoun(mtcars)
  expect_output(print(data), "<pronoun>")
  expect_output(str(data), "<pronoun>")

  data <- as_data_pronoun(list(a = 1))
  expect_output(print(data), "<pronoun>")
})

test_that("data mask can escape", {
  fn <- eval_tidy(quote(function() cyl), mtcars)
  expect_identical(fn(), mtcars$cyl)
})

test_that("inner formulas are evaluated in the current frame", {
  quo <- quo(local(list(f_env = f_env(~foo), env = current_env())))
  envs <- eval_tidy(quo)
  expect_identical(envs$f_env, envs$env)

  quo <- quo(as_function(~list(f_env = get_env(~foo), env = current_env()))())
  envs <- eval_tidy(quo)
  expect_identical(envs$f_env, envs$env)
})

test_that("names are translated to native when creating data mask", {
  with_latin1_locale({
    str_utf8 <- "\u00fc"
    str_native <- enc2native(str_utf8)

    d <- set_names(list("value"), str_utf8)
    s <- sym(str_native)
    expect_identical(eval_tidy(s, data = d), "value")

    foreign_utf8 <- "\u5FCD"
    foreign_native <- enc2native(foreign_utf8)

    d <- setNames(list("value"), foreign_utf8)
    s <- sym(foreign_native)
    expect_identical(eval_tidy(s, data = d), "value")
  })
})

test_that("new_data_mask() checks `top` is a parent of `bottom`", {
  top <- env()
  bottom <- env(top)
  expect_no_error(new_data_mask(bottom, top))
  expect_error(new_data_mask(top, bottom), "`top` is not a parent of `bottom`")
})

test_that("data mask inherits from last environment", {
  mask <- new_data_mask(NULL, NULL)
  expect_reference(env_parent(mask), empty_env())

  eval_tidy(NULL, mask)
  expect_reference(env_parent(mask), current_env())

  env <- env()
  quo <- new_quosure(NULL, env)
  eval_tidy(quo, mask)
  expect_reference(env_parent(mask), env)
})

test_that("is_data_pronoun() detects pronouns", {
  expect_true(!!is_data_pronoun(quote(.data$foo)))
  expect_true(!!is_data_pronoun(quote(.data[[foo]])))
  expect_false(!!is_data_pronoun(quote(.data[foo])))
  expect_false(!!is_data_pronoun(quote(data[[foo]])))
})

test_that("data_pronoun_name() extracts name", {
  expr <- quote(.data[[foo]])
  expect_null(data_pronoun_name(expr))

  expr <- quote(.data[[toupper("foo")]])
  expect_null(data_pronoun_name(expr))

  expr <- quote(`$`(.data, toupper("foo")))
  expect_null(data_pronoun_name(expr))

  expect_identical(data_pronoun_name(quote(.data[["foo"]])), "foo")
  expect_identical(data_pronoun_name(quote(.data$foo)), "foo")
})

test_that(".data pronoun walks the ancestry of environments", {
  e  <- 0
  e1 <- env(a = 1, b = 1, c = 1)
  e2 <- env(a = 2, b = 2, e1)
  e3 <- env(a = 3, e2)

  data_mask <- new_data_mask(e3, e1)
  .data <- as_data_pronoun(data_mask)

  expect_equal(.data$a, 3)
  expect_equal(.data$b, 2)
  expect_equal(.data$c, 1)
  expect_data_pronoun_error(.data$d, "Column `d` not found in `.data`")
  expect_data_pronoun_error(.data$e, "Column `e` not found in `.data`")
  expect_data_pronoun_error(.data$.data, "Column `.data` not found in `.data`")
  expect_data_pronoun_error(.data$.env, "Column `.env` not found in `.data`")
  expect_data_pronoun_error(.data$.top_env, "Column `.top_env` not found in `.data`")

  expect_equal(.data[["a"]], 3)
  expect_equal(.data[["b"]], 2)
  expect_equal(.data[["c"]], 1)
  expect_data_pronoun_error(.data[["d"]], "Column `d` not found in `.data`")
  expect_data_pronoun_error(.data[["e"]], "Column `e` not found in `.data`")
  expect_data_pronoun_error(.data[[".data"]], "Column `.data` not found in `.data`")
  expect_data_pronoun_error(.data[[".env"]], "Column `.env` not found in `.data`")
  expect_data_pronoun_error(.data[[".top_env"]], "Column `.top_env` not found in `.data`")

  expect_error(.data["a"])
})

test_that("can inspect the exported pronoun", {
  expect_output(print(rlang::.data), "<pronoun>")
})

test_that("data pronoun always skips functions", {
  top <- env(c = "c")
  bottom <- env(top, c = base::c)
  mask <- new_data_mask(bottom, top)

  .data <- as_data_pronoun(mask)
  expect_identical(.data$c, "c")
})

test_that("leaked quosure masks are not mistaken with data masks", {
  scoped_lifecycle_silence()
  e <- eval_tidy(quote(current_env()))
  expect_no_error(eval_tidy("foo", e))
})

test_that("quosures look for data masks lexically", {
  out <- eval_tidy(data = mtcars, expr({
    fn <- as_function(~ !!quo(cyl))
    list(
      fn(),
      local(!!quo(disp))
    )
  }))
  expect_identical(out, list(mtcars$cyl, mtcars$disp))
})

test_that("can evaluate quosures created in the data mask without infloop", {
  quo <- eval_tidy(quote(quo(a)), list(a = "foo"))
  expect_identical(eval_bare(quo, quo_get_env(quo)), "foo")
})

test_that("`.env` pronoun is constructed", {
  pronoun <- eval_tidy(quote(.env), mtcars)
  expect_is(pronoun, "rlang_ctxt_pronoun")
  expect_reference(env_parent(pronoun), current_env())
})

test_that("the `.env` pronoun is not an environment", {
  pronoun <- eval_tidy(quote(.env), mtcars)
  expect_error(length(pronoun), "Can't take the")

  skip_if(getRversion() < "3.2")
  expect_error(names(pronoun), "Can't take the")
})

test_that("subsetting `.env` evaluates", {
  expect_error(eval_tidy(quote(.env[["cyl"]]), mtcars, env()), "not found")
  cyl <- "foo"
  expect_identical(eval_tidy(quote(.env$cyl), mtcars, env()), "foo")
  expect_identical(eval_tidy(quote(.env[["cyl"]]), mtcars, env()), "foo")
})

test_that("mask inherits from `env` after evaluation", {
  flag <- env(empty_env())
  mask <- new_data_mask(env())
  eval_tidy(NULL, mask, flag)
  expect_true(env_inherits(mask, flag))
})

test_that("can't take the names() and length() of the `.data` pronoun", {
  pronoun <- as_data_pronoun(mtcars)
  expect_error(names(pronoun), "Can't take")
  expect_error(length(pronoun), "Can't take")
})


# Lifecycle ----------------------------------------------------------

test_that("as_data_mask() and new_data_mask() are deprecated", {
  expect_defunct(as_data_mask(mtcars, env()))
  expect_defunct(new_data_mask(NULL, NULL, parent = env()))
})

test_that("supplying environment as data is deprecated", {
  scoped_options(lifecycle_verbose_soft_deprecation = TRUE)
  `_x` <- "foo"
  expect_warning(eval_tidy("foo", current_env()), "deprecated")
  expect_identical(eval_tidy(quo(`_x`), current_env()), "foo")
  expect_error(eval_tidy(quo(`_y`), current_env()), "not found")
})
