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
    with_handlers(warning = calling(handler),
      .Call(rlang_test_r_warn, "foo")
    ))
})

test_that("r_on_exit() adds deferred expr", {
  var <- chr()
  fn <- function() {
    .Call(rlang_test_r_on_exit, quote(var <<- c(var, "foo")), current_env())
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

test_that("r_base_ns_get() fail if object does not exist", {
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

test_that("r_which_operator() returns correct tokens", {
  expect_identical(which_operator(quote(foo())), "")
  expect_identical(which_operator(""), "")

  expect_identical(which_operator(quote(?a)), "?unary")
  expect_identical(which_operator(quote(a ? b)), "?")

  expect_identical(which_operator(quote(while (a) b)), "while")
  expect_identical(which_operator(quote(for (a in b) b)), "for")
  expect_identical(which_operator(quote(repeat a)), "repeat")
  expect_identical(which_operator(quote(if (a) b)), "if")

  expect_identical(which_operator(quote(a <- b)), "<-")
  expect_identical(which_operator(quote(a <<- b)), "<<-")
  expect_identical(which_operator(quote(a < b)), "<")
  expect_identical(which_operator(quote(a <= b)), "<=")
  expect_identical(which_operator(quote(`<--`(a, b))), "")
  expect_identical(which_operator(quote(`<<--`(a, b))), "")
  expect_identical(which_operator(quote(`<==`(a, b))), "")

  expect_identical(which_operator(quote(a > b)), ">")
  expect_identical(which_operator(quote(a >= b)), ">=")
  expect_identical(which_operator(quote(`>-`(a, b))), "")
  expect_identical(which_operator(quote(`>==`(a, b))), "")

  expect_identical(which_operator(quote(`=`(a, b))), "=")
  expect_identical(which_operator(quote(a == b)), "==")
  expect_identical(which_operator(quote(`=-`(a, b))), "")
  expect_identical(which_operator(quote(`==-`(a, b))), "")

  expect_identical(which_operator(quote(~a)), "~unary")
  expect_identical(which_operator(quote(a ~ b)), "~")
  expect_identical(which_operator(quote(`~-`(a))), "")

  expect_identical(which_operator(quote(a:b)), ":")
  expect_identical(which_operator(quote(a::b)), "::")
  expect_identical(which_operator(quote(a:::b)), ":::")
  expect_identical(which_operator(quote(a := b)), ":=")
  expect_identical(which_operator(quote(`:-`(a, b))), "")
  expect_identical(which_operator(quote(`::-`(a, b))), "")
  expect_identical(which_operator(quote(`:::-`(a, b))), "")
  expect_identical(which_operator(quote(`:=-`(a, b))), "")

  expect_identical(which_operator(quote(a | b)), "|")
  expect_identical(which_operator(quote(a || b)), "||")
  expect_identical(which_operator(quote(`|-`(a, b))), "")
  expect_identical(which_operator(quote(`||-`(a, b))), "")

  expect_identical(which_operator(quote(a & b)), "&")
  expect_identical(which_operator(quote(a && b)), "&&")
  expect_identical(which_operator(quote(`&-`(a, b))), "")
  expect_identical(which_operator(quote(`&&-`(a, b))), "")

  expect_identical_(which_operator(quote(!b)), "!")
  expect_identical_(which_operator(quote(`!!`(b))), "!!")
  expect_identical_(which_operator(quote(`!!!`(b))), "!!!")
  expect_identical_(which_operator(quote(`!-`(a, b))), "")
  expect_identical_(which_operator(quote(`!!-`(a, b))), "")
  expect_identical_(which_operator(quote(`!!!-`(a, b))), "")
  expect_identical_(which_operator(quote(!?b)), "!")
  expect_identical_(which_operator(quote(!!?b)), "!")

  expect_identical(which_operator(quote(+a)), "+unary")
  expect_identical(which_operator(quote(a + b)), "+")
  expect_identical(which_operator(quote(`+-`(a))), "")

  expect_identical(which_operator(quote(-a)), "-unary")
  expect_identical(which_operator(quote(a - b)), "-")
  expect_identical(which_operator(quote(`--`(a))), "")

  expect_identical(which_operator(quote(a * b)), "*")
  expect_identical(which_operator(quote(a / b)), "/")
  expect_identical(which_operator(quote(a ^ b)), "^")
  expect_identical(which_operator(quote(a$b)), "$")
  expect_identical(which_operator(quote(a@b)), "@")
  expect_identical(which_operator(quote(a[b])), "[")
  expect_identical(which_operator(quote(a[[b]])), "[[")
  expect_identical(which_operator(quote(`*-`(a, b))), "")
  expect_identical(which_operator(quote(`/-`(a, b))), "")
  expect_identical(which_operator(quote(`^-`(a, b))), "")
  expect_identical(which_operator(quote(`$-`(a, b))), "")
  expect_identical(which_operator(quote(`@-`(a, b))), "")
  expect_identical(which_operator(quote(`[-`(a, b))), "")
  expect_identical(which_operator(quote(`[[-`(a, b))), "")

  expect_identical(which_operator(quote(a %% b)), "%%")
  expect_identical(which_operator(quote(a %>% b)), "special")
  expect_identical(which_operator(quote(`%%-`(a))), "")

  expect_identical(which_operator(quote((a))), "(")
  expect_identical(which_operator(quote({ a })), "{")
  expect_identical(which_operator(quote(`(-`(a))), "")
  expect_identical(which_operator(quote(`{-`(a))), "")
})

test_that("client library passes tests", {
  skip_on_cran()

  # Silence package building and embedded tests output
  temp <- file()
  sink(temp)
  on.exit({
    sink()
    close(temp)
  })

  # tools::testInstalledPackage() can't find the package if we install
  # to a temporary library
  if (FALSE) {
    old_libpaths <- .libPaths()
    temp_lib <- tempfile("temp_lib")
    dir.create(temp_lib)
    .libPaths(c(temp_lib, old_libpaths))
    on.exit(.libPaths(old_libpaths), add = TRUE)
  } else {
    temp_lib <- .libPaths()
  }

  zip_file <- normalizePath(file.path("fixtures", "lib.zip"))
  src_path <- normalizePath(file.path("fixtures", "rlanglibtest"))

  # Set temporary dir to install and test the embedded package so we
  # don't have to clean leftovers files
  temp_test_dir <- tempfile("temp_test_dir")
  dir.create(temp_test_dir)
  old <- setwd(temp_test_dir)
  on.exit(setwd(old), add = TRUE)

  file.copy(src_path, temp_test_dir, overwrite = TRUE, recursive = TRUE)
  pkg_path <- file.path(temp_test_dir, "rlanglibtest")


  # We store the library as a zip to avoid VCS noise. Use
  # fixtures/Makefile to regenerate it.
  utils::unzip(zip_file, exdir = file.path(pkg_path, "src"))

  install.packages(pkg_path,
    repos = NULL,
    type = "source",
    lib = temp_lib,
    INSTALL_opts = "--install-tests",
    verbose = FALSE,
    quiet = TRUE
  )

  result <- tools::testInstalledPackage("rlanglibtest", lib.loc = temp_lib, types = "test")
  expect_identical(result, 0L)
})

test_that("r_env_unbind() removes objects", {
  env <- env(a = 1L)
  r_env_unbind(env, "a")
  expect_false(env_has(env, "a"))

  env <- env(a = 1L)
  child <- child_env(env)
  expect_warning(r_env_unbind(child, "a"), "not found")
  r_env_unbind(child, "a", inherits = TRUE)
  expect_false(env_has(env, "a"))
})

node_list_clone_until <- function(node, sentinel) {
  .Call(rlang_test_node_list_clone_until, node, sentinel)
}

test_that("can clone-until with NULL list", {
  expect_identical(node_list_clone_until(NULL, pairlist()), list(NULL, NULL))
})

test_that("can clone-until with NULL sentinel", {
  node <- pairlist(a = 1, b = 2, c = 3)
  out <- node_list_clone_until(node, NULL)

  sentinel_out <- out[[2]]
  expect_reference(node_cddr(out[[1]]), sentinel_out)

  node_out <- out[[1]]
  expect_identical(node_out, pairlist(a = 1, b = 2, c = 3))
  while (!is_null(node_out)) {
    expect_false(is_reference(node_out, node))
    node_out <- node_cdr(node_out)
    node <- node_cdr(node)
  }
})

test_that("returned sentinel and value are NULL if couldn't be found", {
  node <- pairlist(a = NULL)
  out <- node_list_clone_until(node, pairlist(NULL))

  expect_false(is_reference(out[[1]], node))
  expect_null(out[[1]])
  expect_null(out[[2]])
})

test_that("can clone until sentinel", {
  node1 <- pairlist(a = 1, b = 2, c = 3)
  node2 <- node_cdr(node1)
  node3 <- node_cdr(node2)

  out <- node_list_clone_until(node1, node2)

  # No modification by reference of original list
  expect_false(is_reference(out, node1))
  expect_true(is_reference(node_cdr(node1), node2))
  expect_true(is_reference(node_cdr(node2), node3))

  node_out <- out[[1]]
  expect_identical(node_out, pairlist(a = 1, b = 2, c = 3))
  expect_false(is_reference(node_out, node1))
  expect_true(is_reference(node_cdr(node_out), node2))
  expect_true(is_reference(node_out, out[[2]]))
})

get_attributes <- function(x) {
  .Call(rlang_get_attributes, x)
}
c_set_attribute <- function(x, name, value) {
  .Call(rlang_test_set_attribute, x, sym(name), value)
}

test_that("r_set_attribute() sets elements", {
  x <- list()
  out1 <- c_set_attribute(x, "foo", 1L)
  attrs1 <- get_attributes(out1)
  expect_identical(attrs1, pairlist(foo = 1L))
  expect_false(is_reference(x, out1))
  expect_null(get_attributes(x))

  out2 <- c_set_attribute(out1, "bar", 2L)
  attrs2 <- get_attributes(out2)
  expect_identical(attrs2, pairlist(bar = 2L, foo = 1L))

  expect_reference(get_attributes(out1), attrs1)
  expect_reference(node_cdr(attrs2), attrs1)
})

test_that("r_set_attribute() zaps one element", {
  x <- structure(list(), foo = 1)
  attrs <- get_attributes(x)
  out <- c_set_attribute(x, "foo", NULL)

  expect_reference(get_attributes(x), attrs)
  expect_null(get_attributes(out))
})

test_that("r_set_attribute() zaps several elements", {
  x <- structure(list(), foo = 1, bar = 2, baz = 3)
  attrs <- get_attributes(x)

  out1 <- c_set_attribute(x, "foo", NULL)
  attrs1 <- get_attributes(out1)

  expect_identical(attrs1, pairlist(bar = 2, baz = 3))
  expect_true(is_reference(attrs1, node_cdr(attrs)))
  expect_true(is_reference(node_cdr(attrs1), node_cddr(attrs)))


  out2 <- c_set_attribute(x, "bar", NULL)
  attrs2 <- get_attributes(out2)

  expect_identical(attrs2, pairlist(foo = 1, baz = 3))
  expect_false(is_reference(attrs2, attrs))
  expect_true(is_reference(node_cdr(attrs2), node_cddr(attrs)))


  out3 <- c_set_attribute(x, "baz", NULL)
  attrs3 <- get_attributes(out3)

  expect_identical(attrs3, pairlist(foo = 1, bar = 2))
  expect_false(is_reference(attrs3, attrs))
  expect_false(is_reference(node_cdr(attrs3), node_cdr(attrs)))
})

test_that("can zap non-existing attributes", {
  x <- list()
  out <- c_set_attribute(x, "foo", NULL)
  expect_identical(out, list())
  expect_false(is_reference(x, out))

  x2 <- structure(list(), foo = 1, bar = 2)
  out2 <- c_set_attribute(x2, "baz", NULL)
  attrs2 <- get_attributes(out2)
  expect_identical(attrs2, pairlist(foo = 1, bar = 2))
  expect_reference(attrs2, get_attributes(x2))
})

test_that("r_parse()", {
  expect_equal(.Call(rlang_test_parse, "{ foo; bar }"), quote({ foo; bar }))
  expect_error(.Call(rlang_test_parse, "foo; bar"), "single expression")
  expect_error(.Call(rlang_test_parse, "foo\n bar"), "single expression")
})

test_that("r_parse_eval()", {
  foo <- "quux"
  expect_identical(r_parse_eval("toupper(foo)"), "QUUX")
  expect_error(r_parse_eval("toupper(foo); foo"), "single expression")
})

test_that("failed parses are printed if `rlang__verbose_errors` is non-NULL", {
  err <- catch_cnd(expect_output(
      regexp =  "foo; bar",
      with_options(rlang__verbose_errors = TRUE,
        .Call(rlang_test_parse, "foo; bar")
      )
    ))
  expect_error(cnd_signal(err), regexp = "single expression")
})

test_that("r_warn_deprecated() warns once", {
  expect_warning(warn_deprecated("retired", "foo"), "retired")
  expect_no_warning(warn_deprecated("retired", "foo"))
  expect_warning(warn_deprecated("retired", "bar"), "retired")
})

test_that("r_nms_are_duplicated() detects duplicates", {
  out <- r_nms_are_duplicated(letters)
  expect_identical(out, rep(FALSE, length(letters)))

  out <- r_nms_are_duplicated(c("a", "b", "a", "a", "c", "c"))
  expect_identical(out, c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))
})

test_that("r_nms_are_duplicated() handles empty and missing names", {
  out <- r_nms_are_duplicated(c("a", NA, NA, "b", "", "", "a"))
  expect_identical(out, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("r_lgl_sum() handles NA", {
  expect_identical(r_lgl_sum(lgl(TRUE, FALSE), TRUE), 1L)
  expect_identical(r_lgl_sum(lgl(TRUE, NA), TRUE), 2L)
  expect_identical(r_lgl_sum(lgl(TRUE, NA), FALSE), 1L)
})

test_that("r_lgl_which() handles NA", {
  expect_identical(r_lgl_which(lgl(TRUE, FALSE), TRUE), 1L)
  expect_identical(r_lgl_which(lgl(TRUE, FALSE), FALSE), 1L)
  expect_identical(r_lgl_which(lgl(TRUE, NA), TRUE), int(1L, NA))
  expect_identical(r_lgl_which(lgl(TRUE, NA), FALSE), 1L)
})

test_that("r_lgl_which() handles empty vectors", {
  expect_identical(r_lgl_which(lgl(), TRUE), int())
  expect_identical(r_lgl_which(lgl(), FALSE), int())
})

test_that("r_lgl_which() handles `NA` when propagation is disabled (#750)", {
  expect_identical(r_lgl_which(lgl(TRUE, FALSE, NA), FALSE), int(1))
  expect_identical(r_lgl_which(lgl(TRUE, FALSE, NA, TRUE), FALSE), int(1, 4))
  expect_identical(r_lgl_which(lgl(TRUE, NA, FALSE, NA, TRUE, FALSE, TRUE), FALSE), int(1, 5, 7))
})

test_that("r_node_list_reverse() reverses destructively", {
  x <- pairlist(1)
  y <- node_list_reverse(x)
  expect_true(is_reference(x, y))

  x <- pairlist(1, 2)
  n1 <- x
  n2 <- node_cdr(x)
  y <- node_list_reverse(x)

  expect_identical(y, pairlist(2, 1))
  expect_true(is_reference(x, n1))
  expect_true(is_reference(y, n2))
  expect_true(is_reference(node_cdr(y), n1))
  expect_true(is_null(node_cdr(n1)))

  x <- pairlist(1, 2, 3)
  n1 <- x
  n2 <- node_cdr(x)
  n3 <- node_cddr(x)
  y <- node_list_reverse(x)

  expect_identical(y, pairlist(3, 2, 1))
  expect_true(is_reference(x, n1))
  expect_true(is_reference(y, n3))
  expect_true(is_reference(node_cdr(y), n2))
  expect_true(is_reference(node_cddr(y), n1))
  expect_true(is_null(node_cdr(n1)))
})
