context("C API")

r_string <- function(str) {
  stopifnot(is_string(str))
  .Call(rlang_r_string, str)
}

test_that("chr_prepend() prepends", {
  out <- .Call(rlang_test_chr_prepend, c("foo", "bar"), r_string("baz"))
  expect_identical(out, c("baz", "foo", "bar"))
})

test_that("chr_append() appends", {
  out <- .Call(rlang_test_chr_append, c("foo", "bar"), r_string("baz"))
  expect_identical(out, c("foo", "bar", "baz"))
})

test_that("r_warn() signals", {
  handler <- function(c) expect_null(c$call)

  expect_warning(regexp = "foo",
    with_handlers(warning = inplace(handler),
      .Call(rlang_test_r_warn, "foo")
    ))
})

test_that("r_on_exit() adds deferred expr", {
  var <- chr()
  fn <- function() {
    .Call(rlang_test_r_on_exit, quote(var <<- c(var, "foo")), get_env())
    var <<- c(var, "bar")
  }
  fn()
  expect_identical(var, c("bar", "foo"))
})
