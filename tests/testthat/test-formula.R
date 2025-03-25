# Creation ----------------------------------------------------------------

test_that("is_formula works", {
  expect_true(is_formula(~10))
  expect_false(is_formula(10))
})


# Getters -----------------------------------------------------------------

test_that("throws errors for bad inputs", {
  expect_error(f_rhs(1), "must be a formula")
  expect_error(f_rhs(`~`()), "Invalid formula")
  expect_error(f_rhs(`~`(1, 2, 3)), "Invalid formula")

  expect_error(f_lhs(1), "must be a formula")
  expect_error(f_lhs(`~`()), "Invalid formula")
  expect_error(f_lhs(`~`(1, 2, 3)), "Invalid formula")

  expect_error(f_env(1), "must be a formula")
})

test_that("extracts call, name, or scalar", {
  expect_identical(f_rhs(~x), quote(x))
  expect_identical(f_rhs(~ f()), quote(f()))
  expect_identical(f_rhs(~1L), 1L)
})


# Setters -----------------------------------------------------------------

test_that("can replace RHS of one-sided formula", {
  f <- ~x1
  f_rhs(f) <- quote(x2)

  expect_equal(f, ~x2)
})

test_that("can replace both sides of two-sided formula", {
  f <- x1 ~ y1
  f_lhs(f) <- quote(x2)
  f_rhs(f) <- quote(y2)

  expect_equal(f, x2 ~ y2)
})

test_that("can remove lhs of two-sided formula", {
  f <- x ~ y
  f_lhs(f) <- NULL

  expect_equal(f, ~y)
})

test_that("can modify environment", {
  f <- x ~ y
  env <- new.env()
  f_env(f) <- env

  expect_equal(f_env(f), env)
})

test_that("setting RHS preserves attributes", {
  attrs <- list(foo = "bar", class = "formula")

  f <- inject(structure(~foo, !!!attrs))
  f_rhs(f) <- quote(bar)

  expect_identical(f, inject(structure(~bar, !!!attrs)))
})

test_that("setting LHS preserves attributes", {
  attrs <- list(foo = "bar", class = "formula")

  f <- inject(structure(~foo, !!!attrs))
  f_lhs(f) <- quote(bar)

  expect_identical(f, inject(structure(bar ~ foo, !!!attrs)))

  f_lhs(f) <- quote(baz)
  expect_identical(f, inject(structure(baz ~ foo, !!!attrs)))
})

test_that("setting environment preserves attributes", {
  attrs <- list(foo = "bar", class = "formula")
  env <- env()

  f <- inject(structure(~foo, !!!attrs))
  f_env(f) <- env
  expect_identical(f, inject(structure(~foo, !!!attrs, .Environment = env)))
})

test_that("unevaluated tilde calls are formulas", {
  f <- quote(~foo)
  expect_true(is_formula(f))
  expect_false(is_formula(f, scoped = TRUE))
  expect_true(is_formula(f, scoped = FALSE))
  expect_true(is_formula(f, scoped = NULL))

  expect_false(is_bare_formula(f))
  expect_false(is_bare_formula(f, scoped = TRUE))
  expect_true(is_bare_formula(f, scoped = FALSE))
  expect_true(is_bare_formula(f, scoped = NULL))
})


# Utils --------------------------------------------------------------

test_that("quosures are not recognised as bare formulas", {
  expect_false(is_bare_formula(quo(foo)))
})

test_that("lhs is inspected", {
  expect_true(is_formula(~foo))

  expect_false(is_formula(~foo, lhs = TRUE))
  expect_true(is_formula(~foo, lhs = FALSE))

  expect_true(is_formula(foo ~ bar, lhs = TRUE))
  expect_false(is_formula(foo ~ bar, lhs = FALSE))
})
