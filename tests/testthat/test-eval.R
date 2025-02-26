test_that("supports tidy dots", {
  expect_equal(exec(list, x = 1), list(x = 1))

  args <- list(x = 1)
  expect_equal(exec(list, !!!args), list(x = 1))
  expect_equal(exec(list, !!!args, y = 2), list(x = 1, y = 2))
})

test_that("does not inline expressions", {
  expect_equal(exec(list, x = expr(x), y = expr(y)), exprs(x = x, y = y))
})

test_that("inject() injects", {
  expect_equal_(
    inject(quote(foo(!!(1:2), !!!1:3))),
    call2("foo", 1:2, !!!1:3)
  )

  g <- function(x) substitute(x)
  f <- function(x) inject(g({{ x }}))
  expect_equal(
    f(foo()),
    quo(foo())
  )
})

test_that("inject() and eval_bare() propagate visibility", {
  expect_invisible(eval_bare(quote(invisible(list()))))
  expect_invisible(inject(invisible(list())))
})
