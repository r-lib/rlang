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

test_that("r_is_special_op_sym() detects special operators", {
  is_special_op <- function(x) .Call(rlang_test_is_special_op_sym, x)
  expect_false(is_special_op(quote(foo)))
  expect_true(is_special_op(quote(`%>%`)))

  expect_false(is_special_op(quote(`%>>`)))
  expect_false(is_special_op(quote(`%%`)))
})

test_that("r_base_ns_get() and r_env_get() fail if object does not exist", {
  expect_error(.Call(rlang_test_base_ns_get, "foobar"))
})

test_that("r_current_frame() returns current frame", {
  current_frame <- function() {
    list(.Call(rlang_test_current_frame), environment())
  }
  out <- current_frame()
  expect_identical(out[[1]], out[[2]])
})

test_that("r_sys_frame() returns current frame environment", {
  sys_frame <- function(..., .n = 0L) {
    list(.Call(rlang_test_sys_frame, .n), sys.frame(.n))
  }
  out <- sys_frame(foo(), bar)
  expect_identical(out[[1]], out[[2]])

  wrapper <- function(...) {
    sys_frame(.n = -1L)
  }
  out <- wrapper(foo(), bar)
  expect_identical(out[[1]], out[[2]])
})

test_that("r_sys_call() returns current frame call", {
  sys_call <- function(..., .n = 0L) {
    list(.Call(rlang_test_sys_call, .n), sys.call(.n))
  }
  out <- sys_call(foo(), bar)
  expect_identical(out[[1]], out[[2]])

  wrapper <- function(...) {
    sys_call(.n = -1L)
  }
  out <- wrapper(foo(), bar)
  expect_identical(out[[1]], out[[2]])
})
