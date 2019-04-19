context("function")

test_that("new_function equivalent to regular function", {
  f1 <- function(x = a + b, y) {
    x + y
  }
  attr(f1, "srcref") <- NULL

  f2 <- new_function(alist(x = a + b, y =), quote({x + y}))

  expect_equal(f1, f2)

  env <- current_env()
  expect_true(is_reference(fn_env(f2), env))
})

test_that("prim_name() extracts names", {
  expect_equal(prim_name(c), "c")
  expect_equal(prim_name(prim_eval), "eval")
})

test_that("as_closure() returns closure", {
  expect_identical(typeof(as_closure(base::list)), "closure")
  expect_identical(typeof(as_closure("list")), "closure")
})

test_that("as_closure() handles primitive functions", {
  expect_identical(as_closure(`c`)(1, 3, 5), c(1, 3, 5))
  expect_identical(as_closure(is.null)(1), FALSE)
  expect_identical(as_closure(is.null)(NULL), TRUE)
})

test_that("as_closure() supports base-style and purrr-style arguments to binary operators", {
  and <- as_closure(`&&`)

  expect_error(and(), "Must supply `e1` or `.x` to binary operator")
  expect_error(and(TRUE), "Must supply `e2` or `.y` to binary operator")

  expect_error(and(.x = TRUE, e1 = TRUE), "Can't supply both `e1` and `.x` to binary operator")
  expect_error(and(TRUE, .y = TRUE, e2 = TRUE), "Can't supply both `e2` and `.y` to binary operator")

  expect_identical(and(FALSE, FALSE), FALSE)
  expect_identical(and(TRUE, FALSE), FALSE)
  expect_identical(and(FALSE, TRUE), FALSE)
  expect_identical(and(TRUE, TRUE), TRUE)

  expect_identical(and(.y = FALSE, TRUE), FALSE)
  expect_identical(and(e2 = FALSE, TRUE), FALSE)
  expect_identical(and(.y = FALSE, e1 = TRUE), FALSE)
  expect_identical(and(e2 = FALSE, .x = TRUE), FALSE)
  expect_identical(and(.y = FALSE, TRUE), FALSE)
  expect_identical(and(e2 = FALSE, TRUE), FALSE)
})

test_that("as_closure() supports base-style and purrr-style arguments to versatile operators", {
  minus <- as_closure(`-`)

  expect_error(minus(), "Must supply `e1` or `.x` to binary operator")
  expect_error(minus(.y = 3), "Must supply `e1` or `.x` to binary operator")

  expect_error(minus(.x = 3, e1 = 1), "Can't supply both `e1` and `.x` to binary operator")
  expect_error(minus(0, .y = 3, e2 = 1), "Can't supply both `e2` and `.y` to binary operator")

  expect_identical(minus(3), -3)
  expect_identical(minus(e1 = 3), -3)
  expect_identical(minus(.x = 3), -3)

  expect_identical(minus(1, 3), -2)
  expect_identical(minus(3, 1), 2)

  expect_identical(minus(.y = 3, 1), -2)
  expect_identical(minus(e2 = 3, 1), -2)
  expect_identical(minus(.y = 3, e1 = 1), -2)
  expect_identical(minus(e2 = 3, .x = 1), -2)
  expect_identical(minus(.y = 1, 3), 2)
  expect_identical(minus(e2 = 1, 3), 2)
})

test_that("as_closure(`||`) shortcircuits", {
  or <- as_closure(`||`)

  expect_error(or(), "Must supply `e1` or `.x` to binary operator")
  expect_error(or(FALSE), "Must supply `e2` or `.y` to binary operator")

  expect_identical(or(TRUE), TRUE)
  expect_identical(or(.x = TRUE), TRUE)
  expect_identical(or(e1 = TRUE), TRUE)
})

test_that("as_closure() handles operators", {
  expect_identical(as_closure(`-`)(.y = 10, .x = 5), -5)
  expect_identical(as_closure(`-`)(5), -5)
  expect_identical(as_closure(`$`)(mtcars, cyl), mtcars$cyl)
  expect_identical(as_closure(`~`)(foo), ~foo)
  expect_identical(as_closure(`~`)(foo, bar), foo ~ bar)
  expect_warning(expect_identical(as_closure(`{`)(warn("foo"), 2, 3), 3), "foo")

  x <- "foo"
  as_closure(`<-`)(x, "bar")
  expect_identical(x, "bar")

  x <- list(a = 1, b = 2)
  as_closure(`$<-`)(x, b, 20)
  expect_identical(x, list(a = 1, b = 20))

  x <- list(1, 2)
  as_closure(`[[<-`)(x, 2, 20)
  expect_identical(x, list(1, 20))

  x <- data.frame(x = 1:2, y = 3:4)
  expect_identical(as_closure(`[<-`)(x, 2, 2, 10L), 10L)
  expect_identical(x, data.frame(x = 1:2, y = c(3L, 10L)))
  expect_error(as_closure(`[<-`)(), "Must supply operands")

  methods::setClass("rlang_test", methods::representation(foo = "character"))
  s4 <- methods::new("rlang_test")
  as_closure(`@<-`)(s4, "foo", "FOO")
  expect_identical(s4@foo, "FOO")

  x <- list(1, 2)
  expect_identical(as_closure(`[[<-`)(x, 2, 20), 20)
  expect_identical(x, list(1, 20))

  x <- list2(list2(a = "A"), list2(a = "B"))
  expect_identical(lapply(x, as_closure(`[[`), "a"), list("A", "B"))
})

test_that("lambda shortcut handles positional arguments", {
  expect_identical(as_function(~ ..1 + ..3)(1, 2, 3), 4)
})

test_that("lambda shortcut fails with two-sided formulas", {
  expect_error(as_function(lhs ~ ..1 + ..3), "two-sided formula")
})

test_that("as_function() handles strings", {
  expect_identical(as_function("mean"), mean)

  env <- env(fn = function() NULL)
  expect_identical(as_function("fn", env), env$fn)
})

test_that("fn_fmls_syms() unnames `...`", {
  expect_identical(fn_fmls_syms(lapply), list(X = quote(X), FUN = quote(FUN), quote(...)))
})

test_that("fn_fmls_syms() works with functions of zero arguments", {
  expect_identical(fn_fmls_syms(function() NULL), list())
})

test_that("as_closure() gives informative error messages on control flow primitives (#158)", {
  expect_error(as_closure(`if`), "Can't coerce the primitive function `if`")
})

test_that("fn_fmls<- and fn_fmls_names<- change formals", {
  fn <- function() NULL
  fn_fmls(fn) <- list(a = 1)
  expect_identical(fn_fmls(fn), pairlist(a = 1))

  fn_fmls_names(fn) <- c("b")
  expect_identical(fn_fmls(fn), pairlist(b = 1))
})

test_that("fn_ functions requires closures", {
  msg <- "must be an R function, not a primitive function"
  expect_error(fn_fmls(`+`), msg)
  expect_error(fn_fmls_names(`+`), msg)
  expect_error(fn_fmls_syms(`+`), msg)
  expect_error(fn_fmls(`+`) <- list(a = 1, b = 2), msg)
  expect_error(fn_fmls_names(`+`) <- c("A", "B"), msg)
})

test_that("assignment methods preserve attributes", {
  orig <- structure(function(foo) NULL, foo = "foo", bar = "bar")

  fn <- orig
  fn_fmls(fn) <- list(arg = 1)
  expect_identical(attributes(fn), attributes(orig))

  fn <- orig
  fn_fmls_names(fn) <- "bar"
  expect_identical(attributes(fn), attributes(orig))

  fn <- orig
  fn_body(fn) <- "body"
  orig_attrs <- attributes(orig)
  orig_attrs$srcref <- NULL
  expect_identical(attributes(fn), orig_attrs)
})

test_that("print method for `fn` discards attributes", {
  fn <- structure(function() NULL, foo = "foo")
  fn <- new_fn(fn)

  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })

  print(fn)

  output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")
  expect_false(grepl("attr", output))
})

test_that("fn_body() requires a closure to extract body", {
  expect_error(fn_body(c), "`fn` is not a closure")
  expect_equal(fn_body(function() { NULL }), quote({ NULL }))
  expect_equal(fn_body(function() NULL), quote({ NULL }))
})

test_that("fn_env() requires a function to extract env", {
  expect_error(fn_env(1L), "`fn` is not a function")
  expect_identical(fn_env(function() NULL), current_env())
})

test_that("`fn_env<-`() sets environment", {
  fn <- function() NULL
  fn_env(fn) <- base_env()
  expect_reference(fn_env(fn), base_env())
})

test_that("primitive predicates work", {
  expect_true(is_primitive_eager(c))
  expect_true(is_primitive_lazy(`$`))
  expect_false(is_primitive_eager(`$`))
  expect_false(is_primitive_lazy(`c`))
})

test_that("quosures converted to functions ignore their arguments", {
  fn <- as_function(quo("foo"))
  expect_no_error(expect_identical(fn(NULL), "foo"))
})

test_that("as_function() supports nested quosures", {
  quo <- local({
    lhs <- "quux"
    rhs <- local({ rhs <- "hunoz"; quo(rhs) })
    quo(paste(lhs, !!rhs))
  })

  fn <- as_function(quo)
  expect_identical(fn(), "quux hunoz")
})

test_that("fn_body() always returns a `{` block", {
  expect_equal(fn_body(function() "foo"), quote({ "foo" }))
})

test_that("as_function() adds a class to lambda functions", {
  out <- as_function(~foo)
  expect_is(out, c("rlang_lambda_function", "function"))
  expect_output(print(out), "<lambda>")
})

test_that("fn_env() returns base namespace for primitives", {
  expect_reference(fn_env(base::list), ns_env("base"))
})

test_that("as_closure() wrappers dispatch properly", {
  scoped_bindings(.env = global_env(),
    as.character.foobar = function(...) "dispatched!"
  )
  x <- structure(list(), class = "foobar")
  expect_identical(as_closure(as.character)(x), "dispatched!")
})

test_that("as_closure() wrappers are not masked", {
  scoped_bindings(
    .env = global_env(),
    as.character = function(...) abort("tilt")
  )
  expect_identical(as_closure(as.character)(1), "1")
})

test_that("arguments of closured primitives are matched by name before `...` (tidyverse/purrr#411)", {
  expect_false(as_closure(isS4)("foo"))
})

test_that("arguments of closured primitives are matched by name after `...`", {
  fn <- as_closure(min)
  expect_true(is_na(fn(1, NA)))
  expect_identical(fn(na.rm = TRUE, 1, NA), 1)
})
