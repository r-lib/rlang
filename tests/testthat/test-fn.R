context("function")

test_that("new_function equivalent to regular function", {
  f1 <- function(x = a + b, y) {
    x + y
  }
  attr(f1, "srcref") <- NULL

  f2 <- new_function(alist(x = a + b, y =), quote({x + y}))

  expect_equal(f1, f2)
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

  eval_prim <- eval(quote(sys.function()))
  eval_clos <- as_closure(eval_prim)
  expect_identical(typeof(eval_clos), "closure")
  expect_identical(eval_clos(quote(data.frame), base_env()), data.frame)
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

test_that("fn_fmls<- and fn_fmls_names<- handle primitive functions", {
  fn_fmls(`+`) <- list(a = 1, b = 2)
  expect_true(is_closure(`+`))
  expect_identical(fn_fmls(`+`), pairlist(a = 1, b = 2))

  fn_fmls_names(`+`) <- c("A", "B")
  expect_identical(fn_fmls(`+`), pairlist(A = 1, B = 2))
})

test_that("assignment methods preserve attributes", {
  orig <- set_attrs(function(foo) NULL, foo = "foo", bar = "bar")

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
  fn <- set_attrs(function() NULL, foo = "foo")
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
  expect_null(fn_body(function() NULL))
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
