context("dots")

test_that("exprs() without arguments creates an empty named list", {
  expect_identical(exprs(), named_list())
})

test_that("exprs() captures arguments forwarded with `...`", {
  wrapper <- function(...) exprs(...)
  expect_identical(wrapper(a = 1, foo = bar), list(a = 1, foo = quote(bar)))
})

test_that("exprs() captures empty arguments", {
  expect_identical(exprs(, , .ignore_empty = "none"), set_names(list(missing_arg(), missing_arg()), c("", "")))
})

test_that("dots are always named", {
  expect_named(dots_list("foo"), "")
  expect_named(dots_splice("foo", list("bar")), c("", ""))
  expect_named(exprs(foo, bar), c("", ""))
})

test_that("dots can be spliced", {
  spliced_dots <- dots_values(!!! list(letters))
  expect_identical(spliced_dots, list(splice(list(letters))))
  expect_identical(flatten(dots_values(!!! list(letters))), list(letters))
  expect_identical(list2(!!! list(letters)), list(letters))
  wrapper <- function(...) list2(...)
  expect_identical(wrapper(!!! list(letters)), list(letters))
})

test_that("interpolation by value does not guard formulas", {
  expect_identical(dots_values(~1), list(~1))
})

test_that("dots names can be unquoted", {
  expect_identical(dots_values(!! paste0("foo", "bar") := 10), list(foobar = 10))
})

test_that("can take forced dots with `allowForced = FALSE`", {
  fn <- function(...) {
    force(..1)
    captureDots()
  }
  expect_identical(fn(a = letters), pairlist(a = list(expr = letters, env = empty_env())))
})

test_that("captured dots are only named if names were supplied", {
  fn <- function(...) captureDots()
  expect_null(names(fn(1, 2)))
  expect_identical(names(fn(a = 1, 2)), c("a", ""))
})

test_that("dots_values() handles forced dots", {
  fn <- function(...) {
    force(..1)
    dots_values(...)
  }
  expect_identical(fn("foo"), list("foo"))

  expect_identical(lapply(1:2, function(...) dots_values(...)), list(list(1L), list(2L)))
  expect_identical(lapply(1:2, dots_values), list(list(1L), list(2L)))
})

test_that("empty arguments trigger meaningful error", {
  expect_error(list2(1, , 3), "Argument 2 is empty")
  expect_error(dots_list(1, , 3), "Argument 2 is empty")
})

test_that("cleans empty arguments", {
  expect_identical(dots_list(1, ), named_list(1))
  expect_identical(list2(1, ), list(1))
  expect_identical(exprs(1, ), named_list(1))
  expect_identical(dots_list(, 1, , .ignore_empty = "all"), named_list(1))
})

test_that("doesn't clean named empty argument arguments", {
  expect_error(dots_list(1, a = ), "Argument 2 is empty")
  expect_identical(exprs(1, a = ), alist(1, a = ))
  expect_identical(exprs(1, a = , b = , , .ignore_empty = "all"), alist(1, a = , b = ))
})

test_that("capturing dots by value only unquote-splices at top-level", {
  expect_identical_(dots_list(!!! list(quote(!!! a))), named_list(quote(!!! a)))
  expect_identical_(dots_list(!!! exprs(!!! 1:3)), named_list(1L, 2L, 3L))
})

test_that("can't unquote when capturing dots by value", {
  expect_identical(dots_list(!!! list(!!! TRUE)), named_list(FALSE))
})

test_that("can splice NULL value", {
  expect_identical(dots_list(!!! NULL), named_list())
  expect_identical(dots_list(1, !!! NULL, 3), named_list(1, 3))
})

test_that("dots_splice() flattens lists", {
  expect_identical(dots_splice(list("a", list("b"), "c"), "d", list("e")), named_list("a", list("b"), "c", "d", "e"))
  expect_identical(dots_splice(list("a"), !!! list("b"), list("c"), "d"), named_list("a", "b", "c", "d"))
  expect_identical(dots_splice(list("a"), splice(list("b")), list("c"), "d"), named_list("a", "b", "c", "d"))
})

test_that("dots_splice() doesn't squash S3 objects", {
  s <- structure(list(v1 = 1, v2 = 2), class = "foo")
  expect_identical(dots_splice(s, s), named_list(s, s))
})

test_that("dots_node() doesn't trim attributes from arguments", {
  x <- ~foo
  dots <- eval(expr(dots_node(!! x)))
  expect_identical(node_car(dots), x)
})

test_that("dots_split() splits named and unnamed dots", {
  dots <- dots_split(1, 2)
  expect_identical(dots$named, list())
  expect_identical(dots$unnamed, list(1, 2))

  dots <- dots_split(a = 1, 2)
  expect_identical(dots$named, list(a = 1))
  expect_identical(dots$unnamed, list(2))

  dots <- dots_split(a = 1, b = 2)
  expect_identical(dots$named, list(a = 1, b = 2))
  expect_identical(dots$unnamed, list())
})

test_that("dots_split() handles empty dots", {
  dots <- dots_split()
  expect_identical(dots$named, list())
  expect_identical(dots$unnamed, list())
})

test_that("dots_split() fails if .n_unnamed doesn't match", {
  expect_error(dots_split(1, 2, .n_unnamed = 1), "Expected 1 unnamed")
  expect_error(dots_split(1, 2, .n_unnamed = 0:1), "Expected 0 or 1 unnamed")

  dots <- dots_split(a = 1, 2, .n_unnamed = 1)
  expect_identical(dots$named, list(a = 1))
  expect_identical(dots$unnamed, list(2))
})

test_that("can splice NULL and atomic vectors", {
  expect_identical(list2(!!!letters), as.list(letters))
  expect_identical(list2(!!!NULL), list())
})

test_that("can unquote quosures in LHS", {
  quo <- quo(foo)
  expect_identical(list2(!!quo := NULL), list(foo = NULL))
  expect_identical(exprs(!!quo := bar), exprs(foo = bar))
})

test_that("can preserve empty arguments", {
  list3 <- function(...) unname(dots_list(..., .preserve_empty = TRUE))
  expect_identical(list3(, ), list(missing_arg()))
  expect_identical(list3(, , .ignore_empty = "none"), list(missing_arg(), missing_arg()))
  expect_identical(list3(, , .ignore_empty = "all"), list())
})

test_that("forced symbolic objects are not evaluated", {
  x <- list(quote(`_foo`))
  expect_identical_(lapply(x, list2), list(x))
  expect_identical_(list2(!!!x), x)

  x <- unname(exprs(stop("tilt")))
  expect_identical_(lapply(x, list2), list(x))
})

test_that("dots collectors do not warn by default with bare `<-` arguments", {
  expect_no_warning(list2(a <- 1))
  expect_no_warning(dots_list(a <- 1))

  expect_no_warning(exprs(a <- 1))
  expect_no_warning(quos(a <- 1))

  myexprs <- function(...) enexprs(...)
  myquos <- function(...) enexprs(...)
  expect_no_warning(myexprs(a <- 1))
  expect_no_warning(myquos(a <- 1))
})

test_that("dots collectors can elect to warn with bare `<-` arguments", {
  expect_warning(dots_list(a <- 1, .check_assign = TRUE), "`<-` as argument")
  myexprs <- function(...) enexprs(..., .check_assign = TRUE)
  myquos <- function(...) enexprs(..., .check_assign = TRUE)
  expect_warning(myexprs(TRUE, a <- 1), "`<-` as argument")
  expect_warning(myquos(TRUE, a <- 1), "`<-` as argument")
})

test_that("dots collectors never warn for <- when option is set", {
  scoped_options(rlang_dots_disable_assign_warning = TRUE)

  expect_no_warning(list2(a <- 1))
  myexprs <- function(...) enexprs(..., .check_assign = TRUE)
  myquos <- function(...) enquos(..., .check_assign = TRUE)
  expect_no_warning(myexprs(a <- 1))
  expect_no_warning(myquos(a <- 1))
})

test_that("`.homonyms` is matched exactly", {
  expect_error(dots_list(.homonyms = "k"), "must be one of")
})

test_that("`.homonyms = 'first'` matches first homonym", {
  list_first <- function(...) {
    dots_list(..., .homonyms = "first")
  }

  out <- list_first(1, 2)
  expect_identical(out, named_list(1, 2))

  out <- list_first(a = 1, b = 2, 3, 4)
  expect_identical(out, list(a = 1, b = 2, 3, 4))

  out <- list_first(a = 1, b = 2, a = 3, a = 4, 5, 6)
  expect_identical(out, list(a = 1, b = 2, 5, 6))
})

test_that("`.homonyms = 'last'` matches last homonym", {
  list_last <- function(...) {
    dots_list(..., .homonyms = "last")
  }

  out <- list_last(1, 2)
  expect_identical(out, named_list(1, 2))

  out <- list_last(a = 1, b = 2, 3, 4)
  expect_identical(out, list(a = 1, b = 2, 3, 4))

  out <- list_last(a = 1, b = 2, a = 3, a = 4, 5, 6)
  expect_identical(out, list(b = 2, a = 4, 5, 6))
})

test_that("`.homonyms` = 'error' fails with homonyms", {
  list_error <- function(...) {
    dots_list(..., .homonyms = "error")
  }

  expect_identical(list_error(1, 2), named_list(1, 2))
  expect_identical(list_error(a = 1, b = 2), list(a = 1, b = 2))

  expect_error(list_error(1, a = 2, a = 3), "multiple arguments named `a` at positions 2 and 3")

  expect_error(list_error(1, a = 2, b = 3, 4, b = 5, b = 6, 7, a = 8), "\\* Multiple arguments named `a` at positions 2 and 8")
  expect_error(list_error(1, a = 2, b = 3, 4, b = 5, b = 6, 7, a = 8), "\\* Multiple arguments named `b` at positions 3, 5, and 6")
})

test_that("`.homonyms` works with spliced arguments", {
  args <- list(a = 1, b = 2, a = 3, a = 4, 5, 6)
  expect_identical(dots_list(!!!args, .homonyms = "first"), list(a = 1, b = 2, 5, 6))

  myexprs <- function(...) enexprs(..., .homonyms = "last")
  expect_identical(myexprs(!!!args), list(b = 2, a = 4, 5, 6))

  myquos <- function(...) enquos(..., .homonyms = "first")
  expect_identical(myquos(!!!args), quos_list(a = quo(1), b = quo(2), quo(5), quo(6)))
})

test_that("can mix `!!!` and splice boxes", {
  expect_identical(list2(1L, !!!(2:3), splice(list(4L))), as.list(1:4))
})

test_that("list2() and dots_values() support splice boxes", {
  expect_identical(list2(1, splice(c("foo", "bar")), 3), list(1, "foo", "bar", 3))
  expect_identical(dots_values(1, splice(c("foo", "bar")), 3), list(1, splice(list("foo", "bar")), 3))
})

test_that("dots_values() doesn't splice", {
  expect_identical_(dots_values(!!!c(1:3)), list(splice(as.list(1:3))))
  expect_identical_(dots_values(!!!list("foo", "bar")), list(splice(list("foo", "bar"))))
})
