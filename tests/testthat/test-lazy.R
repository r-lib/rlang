context("lazy")


test_that("lazy() does not unpack lazily loaded objects", {
  fun <- function(arg) {
    lazyeval::lazy(arg)
  }
  lazy <- fun(mean)
  expect_equal(deparse(lazy$expr), "mean")

  fun2 <- function(arg) {
    fun(arg)
  }
  nested_lazy <- fun2(mean)
  expect_equal(deparse(lazy$expr), "mean")

  fun3 <- function() {
    list(
      lazy = fun(mean),
      env = environment()
    )
  }
  embedded_lazy <- fun3()
  expect_identical(embedded_lazy$lazy$expr, as.name("mean"))
  expect_identical(embedded_lazy$lazy$env, embedded_lazy$env)
})
