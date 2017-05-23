context("C API")

test_that("chr_prepend() prepends", {
  out <- .Call(rlang_test_chr_prepend, c("foo", "bar"), node_cdr("baz"))
  expect_identical(out, c("baz", "foo", "bar"))
})

test_that("chr_append() appends", {
  out <- .Call(rlang_test_chr_append, c("foo", "bar"), node_cdr("baz"))
  expect_identical(out, c("foo", "bar", "baz"))
})

test_that("r_warn() signals", {
  handler <- function(c) expect_null(c$call)

  expect_warning(regexp = "foo",
    with_handlers(warning = inplace(handler),
      .Call(rlang_test_r_warn, "foo")
    ))
})
