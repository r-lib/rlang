# https://github.com/r-lib/rlang/issues/1556
skip_if_not(has_size_one_bool())

r_string <- function(str) {
  stopifnot(is_string(str))
  .Call(ffi_r_string, str)
}

test_that("chr_prepend() prepends", {
  out <- .Call(ffi_test_chr_prepend, c("foo", "bar"), r_string("baz"))
  expect_identical(out, c("baz", "foo", "bar"))
})

test_that("chr_append() appends", {
  out <- .Call(ffi_test_chr_append, c("foo", "bar"), r_string("baz"))
  expect_identical(out, c("foo", "bar", "baz"))
})

test_that("r_warn() signals", {
  expect_warning(regexp = "foo",
    withCallingHandlers(warning = function(c) expect_null(c$call),
      .Call(ffi_test_r_warn, "foo")
    ))
})

test_that("r_on_exit() adds deferred expr", {
  var <- chr()
  fn <- function() {
    .Call(ffi_test_r_on_exit, quote(var <<- c(var, "foo")), current_env())
    var <<- c(var, "bar")
  }
  fn()
  expect_identical(var, c("bar", "foo"))
})

test_that("r_base_ns_get() fail if object does not exist", {
  expect_error(.Call(ffi_test_base_ns_get, "foobar"))
})

test_that("r_peek_frame() returns current frame", {
  current_frame <- function() {
    list(.Call(ffi_test_current_frame), environment())
  }
  out <- current_frame()
  expect_identical(out[[1]], out[[2]])
})

test_that("r_sys_frame() returns current frame environment", {
  sys_frame <- function(..., .n = 0L) {
    list(.Call(ffi_test_sys_frame, .n), sys.frame(.n))
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
    list(.Call(ffi_test_sys_call, .n), sys.call(.n))
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
  expect_identical(call_parse_type(quote(foo())), "")
  expect_identical(call_parse_type(""), "")

  expect_identical(call_parse_type(quote(?a)), "?unary")
  expect_identical(call_parse_type(quote(a ? b)), "?")

  expect_identical(call_parse_type(quote(while (a) b)), "while")
  expect_identical(call_parse_type(quote(for (a in b) b)), "for")
  expect_identical(call_parse_type(quote(repeat a)), "repeat")
  expect_identical(call_parse_type(quote(if (a) b)), "if")
  expect_identical(call_parse_type(quote(break)), "break")
  expect_identical(call_parse_type(quote(next)), "next")

  expect_identical(call_parse_type(quote(a <- b)), "<-")
  expect_identical(call_parse_type(quote(a <<- b)), "<<-")
  expect_identical(call_parse_type(quote(a < b)), "<")
  expect_identical(call_parse_type(quote(a <= b)), "<=")
  expect_identical(call_parse_type(quote(`<--`(a, b))), "")
  expect_identical(call_parse_type(quote(`<<--`(a, b))), "")
  expect_identical(call_parse_type(quote(`<==`(a, b))), "")

  expect_identical(call_parse_type(quote(a > b)), ">")
  expect_identical(call_parse_type(quote(a >= b)), ">=")
  expect_identical(call_parse_type(quote(`>-`(a, b))), "")
  expect_identical(call_parse_type(quote(`>==`(a, b))), "")

  expect_identical(call_parse_type(quote(`=`(a, b))), "=")
  expect_identical(call_parse_type(quote(a == b)), "==")
  expect_identical(call_parse_type(quote(`=-`(a, b))), "")
  expect_identical(call_parse_type(quote(`==-`(a, b))), "")

  expect_identical(call_parse_type(quote(~a)), "~unary")
  expect_identical(call_parse_type(quote(a ~ b)), "~")
  expect_identical(call_parse_type(quote(`~-`(a))), "")

  expect_identical(call_parse_type(quote(a:b)), ":")
  expect_identical(call_parse_type(quote(a::b)), "::")
  expect_identical(call_parse_type(quote(a:::b)), ":::")
  expect_identical(call_parse_type(quote(a := b)), ":=")
  expect_identical(call_parse_type(quote(`:-`(a, b))), "")
  expect_identical(call_parse_type(quote(`::-`(a, b))), "")
  expect_identical(call_parse_type(quote(`:::-`(a, b))), "")
  expect_identical(call_parse_type(quote(`:=-`(a, b))), "")

  expect_identical(call_parse_type(quote(a | b)), "|")
  expect_identical(call_parse_type(quote(a || b)), "||")
  expect_identical(call_parse_type(quote(`|-`(a, b))), "")
  expect_identical(call_parse_type(quote(`||-`(a, b))), "")

  expect_identical(call_parse_type(quote(a & b)), "&")
  expect_identical(call_parse_type(quote(a && b)), "&&")
  expect_identical(call_parse_type(quote(`&-`(a, b))), "")
  expect_identical(call_parse_type(quote(`&&-`(a, b))), "")

  expect_identical_(call_parse_type(quote(!b)), "!")
  expect_identical_(call_parse_type(quote(`!!`(b))), "!!")
  expect_identical_(call_parse_type(quote(`!!!`(b))), "!!!")
  expect_identical_(call_parse_type(quote(`!-`(a, b))), "")
  expect_identical_(call_parse_type(quote(`!!-`(a, b))), "")
  expect_identical_(call_parse_type(quote(`!!!-`(a, b))), "")
  expect_identical_(call_parse_type(quote(!?b)), "!")
  expect_identical_(call_parse_type(quote(!!?b)), "!")

  expect_identical(call_parse_type(quote(+a)), "+unary")
  expect_identical(call_parse_type(quote(a + b)), "+")
  expect_identical(call_parse_type(quote(`+-`(a))), "")

  expect_identical(call_parse_type(quote(-a)), "-unary")
  expect_identical(call_parse_type(quote(a - b)), "-")
  expect_identical(call_parse_type(quote(`--`(a))), "")

  expect_identical(call_parse_type(quote(a * b)), "*")
  expect_identical(call_parse_type(quote(a / b)), "/")
  expect_identical(call_parse_type(quote(a ^ b)), "^")
  expect_identical(call_parse_type(quote(a$b)), "$")
  expect_identical(call_parse_type(quote(a@b)), "@")
  expect_identical(call_parse_type(quote(a[b])), "[")
  expect_identical(call_parse_type(quote(a[[b]])), "[[")
  expect_identical(call_parse_type(quote(`*-`(a, b))), "")
  expect_identical(call_parse_type(quote(`/-`(a, b))), "")
  expect_identical(call_parse_type(quote(`^-`(a, b))), "")
  expect_identical(call_parse_type(quote(`$-`(a, b))), "")
  expect_identical(call_parse_type(quote(`@-`(a, b))), "")
  expect_identical(call_parse_type(quote(`[-`(a, b))), "")
  expect_identical(call_parse_type(quote(`[[-`(a, b))), "")

  expect_identical(call_parse_type(quote(a %% b)), "%%")
  expect_identical(call_parse_type(quote(a %>% b)), "special")
  expect_identical(call_parse_type(quote(`%%-`(a))), "")

  expect_identical(call_parse_type(quote((a))), "(")
  expect_identical(call_parse_type(quote({ a })), "{")
  expect_identical(call_parse_type(quote(`(-`(a))), "")
  expect_identical(call_parse_type(quote(`{-`(a))), "")
})

test_that("client library passes tests", {
  expect_true(TRUE)
  return("Disabled")

  # Avoid installing into system library by default
  skip_if(!nzchar(Sys.getenv("RLANG_FULL_TESTS")))

  skip_on_cran()
  skip_on_ci()

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

node_list_clone_until <- function(node, sentinel) {
  .Call(ffi_test_node_list_clone_until, node, sentinel)
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
  .Call(ffi_attrib, x)
}
c_set_attribute <- function(x, name, value) {
  .Call(ffi_test_attrib_set, x, sym(name), value)
}

test_that("r_attrib_set() sets elements", {
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

test_that("r_attrib_set() zaps one element", {
  x <- structure(list(), foo = 1)
  attrs <- get_attributes(x)
  out <- c_set_attribute(x, "foo", NULL)

  expect_reference(get_attributes(x), attrs)
  expect_null(get_attributes(out))
})

test_that("r_attrib_set() zaps several elements", {
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
  expect_equal(.Call(ffi_test_parse, "{ foo; bar }"), quote({ foo; bar }))
  expect_error(.Call(ffi_test_parse, "foo; bar"), "single expression")
  expect_error(.Call(ffi_test_parse, "foo\n bar"), "single expression")
})

test_that("r_parse_eval()", {
  foo <- "quux"
  expect_identical(r_parse_eval("toupper(foo)"), "QUUX")
  expect_error(r_parse_eval("toupper(foo); foo"), "single expression")
})

test_that("failed parses are printed if `rlang__verbose_errors` is non-NULL", {
  expect_error(
    expect_output(
      regexp =  "foo; bar",
      with_options(rlang__verbose_errors = TRUE,
        .Call(ffi_test_parse, "foo; bar")
      )
    ),
    "single expression"
  )
})

test_that("r_deprecate_warn() warns once", {
  expect_warning(deprecate_warn("retired", "foo"), "retired")
  expect_no_warning(deprecate_warn("retired", "foo"))
  expect_warning(deprecate_warn("retired", "bar"), "retired")
})

test_that("nms_are_duplicated() detects duplicates", {
  out <- nms_are_duplicated(letters)
  expect_identical(out, rep(FALSE, length(letters)))

  out <- nms_are_duplicated(c("a", "b", "a", "a", "c", "c"))
  expect_identical(out, c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))
})

test_that("nms_are_duplicated() handles empty and missing names", {
  out <- nms_are_duplicated(c("a", NA, NA, "b", "", "", "a"))
  expect_identical(out, c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("r_lgl_sum() handles NA", {
  expect_identical(r_lgl_sum(lgl(TRUE, FALSE), TRUE), 1L)
  expect_identical(r_lgl_sum(lgl(TRUE, FALSE), FALSE), 1L)
  expect_identical(r_lgl_sum(lgl(TRUE, NA), TRUE), 2L)
  expect_identical(r_lgl_sum(lgl(TRUE, NA), FALSE), 1L)
})

test_that("r_lgl_which() handles NA", {
  expect_identical(r_lgl_which(lgl(TRUE, FALSE), TRUE), 1L)
  expect_identical(r_lgl_which(lgl(TRUE, FALSE), FALSE), 1L)
  expect_identical(r_lgl_which(lgl(TRUE, NA, FALSE, NA, TRUE, NA), TRUE), int(1L, NA, NA, 5L, NA))
  expect_identical(r_lgl_which(lgl(TRUE, NA, FALSE, NA, TRUE, NA), FALSE), int(1L, 5L))
})

test_that("r_lgl_which() handles empty vectors", {
  expect_identical(r_lgl_which(lgl(), TRUE), int())
  expect_identical(r_lgl_which(lgl(), FALSE), int())

  expect_identical(r_lgl_which(named(lgl()), TRUE), named(int()))
  expect_identical(r_lgl_which(named(lgl()), FALSE), named(int()))
})

test_that("r_lgl_which() propagates names", {
  x <- lgl(a = TRUE, b = FALSE, c = NA, d = FALSE, e = NA, f = TRUE)
  expect_named(r_lgl_which(x, na_propagate = TRUE), c("a", "c", "e", "f"))
  expect_named(r_lgl_which(x, na_propagate = FALSE), c("a", "f"))

  # Unnamed if input is unnamed
  expect_named(r_lgl_which(TRUE, na_propagate = TRUE), NULL)
  expect_named(r_lgl_which(lgl(TRUE, NA), na_propagate = TRUE), NULL)
})

test_that("r_lgl_which() handles `NA` when propagation is disabled (#750)", {
  expect_identical(r_lgl_which(lgl(TRUE, FALSE, NA), FALSE), int(1))
  expect_identical(r_lgl_which(lgl(TRUE, FALSE, NA, TRUE), FALSE), int(1, 4))
  expect_identical(r_lgl_which(lgl(TRUE, NA, FALSE, NA, TRUE, FALSE, TRUE), FALSE), int(1, 5, 7))
})

test_that("r_pairlist_rev() reverses destructively", {
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

test_that("r_dict_put() hashes object", {
  dict <- new_dict(10L)

  expect_true(dict_put(dict, quote(foo), 1))
  expect_true(dict_put(dict, quote(bar), 2))

  expect_false(dict_put(dict, quote(foo), 2))
  expect_false(dict_put(dict, quote(bar), 2))
})

test_that("key has reference semantics", {
  dict <- new_dict(10L)
  keys <- c("foo", "bar")

  # Fresh character vector returned by `[[`
  expect_true(dict_put(dict, keys[[1]], 1))
  expect_true(dict_put(dict, keys[[1]], 2))

  # CHARSXP are interned and unique
  expect_true(dict_put(dict, chr_get(keys[[1]], 0L), 3))
  expect_false(dict_put(dict, chr_get(keys[[1]], 0L), 4))
})

test_that("key can be `NULL`", {
  dict <- new_dict(10L)
  expect_true(dict_put(dict, NULL, 1))
  expect_false(dict_put(dict, NULL, 2))
})

test_that("collisions are handled", {
  dict <- new_dict(1L, prevent_resize = TRUE)

  expect_true(dict_put(dict, quote(foo), 1))
  expect_true(dict_put(dict, quote(bar), 2))
  expect_false(dict_put(dict, quote(bar), 3))

  # Check that dictionary was not resized and we indeed have colliding
  # elements
  expect_equal(dict_size(dict), 1L)
})

test_that("can check existing and retrieve values", {
  dict <- new_dict(10L)

  dict_put(dict, quote(foo), 1)
  dict_put(dict, quote(bar), 2)
  dict_put(dict, quote(foo), 3)

  expect_true(dict_has(dict, quote(foo)))
  expect_true(dict_has(dict, quote(bar)))
  expect_false(dict_has(dict, quote(baz)))

  expect_equal(dict_get(dict, quote(foo)), 1)
  expect_equal(dict_get(dict, quote(bar)), 2)
  expect_error(dict_get(dict, quote(baz)), "Can't find key")
})

test_that("dictionary size is rounded to next power of 2", {
  dict <- new_dict(3L)
  expect_equal(dict_size(dict), 4L)
})

test_that("can resize dictionary", {
  dict <- new_dict(3L)
  dict_resize(dict, 5L)
  expect_equal(dict_size(dict), 8L)
})

test_that("dictionary grows", {
  dict <- new_dict(3L)

  dict_put(dict, quote(foo), 1)
  dict_put(dict, quote(bar), 2)
  dict_put(dict, quote(baz), 3)
  expect_equal(dict_size(dict), 4L)

  dict_put(dict, quote(quux), 4)
  expect_equal(dict_size(dict), 8L)
})

test_that("can delete elements from dict", {
  dict <- new_dict(3L)

  dict_put(dict, quote(foo), 1)
  dict_put(dict, quote(bar), 2)

  expect_true(dict_del(dict, quote(bar)))
  expect_false(dict_has(dict, quote(bar)))
  expect_false(dict_del(dict, quote(bar)))

  expect_true(dict_del(dict, quote(foo)))
  expect_false(dict_has(dict, quote(foo)))
  expect_false(dict_del(dict, quote(foo)))
})

test_that("can put again after del", {
  dict <- new_dict(3L)

  dict_put(dict, quote(foo), 1)
  dict_del(dict, quote(foo))

  expect_true(dict_put(dict, quote(foo), 2))
  expect_equal(dict_get(dict, quote(foo)), 2)


  # Used to fail because we deleted whole bucket instead of just a
  # node when this node appeared first in the bucket

  dict <- new_dict(3L)
  dict_put(dict, chr_get("1"), NULL)
  dict_put(dict, chr_get("foo"), NULL)
  unclass(dict)[[2]]

  dict_del(dict, chr_get("1"))
  unclass(dict)[[2]]

  dict_put(dict, chr_get("1"), "1")

  unclass(dict)

  expect_null(dict_get(dict, chr_get("foo")))
  expect_equal(dict_get(dict, chr_get("1")), "1")
})

test_that("can poke dict value", {
  dict <- new_dict(3L)

  expect_equal(
    dict_poke(dict, quote(foo), 1),
    sym(".__C_NULL__.")
  )
  expect_equal(
    dict_get(dict, quote(foo)),
    1
  )
  expect_equal(
    dict_poke(dict, quote(foo), 2),
    1
  )
  expect_equal(
    dict_get(dict, quote(foo)),
    2
  )
})

test_that("can iterate over dict", {
  dict <- new_dict(10L)

  dict_put(dict, quote(foo), 1)
  dict_put(dict, quote(bar), 2)

  it <- new_dict_iterator(dict)
  expect_equal(
    dict_it_info(it),
    list(
      key = NULL,
      value = NULL,
      i = 0L,
      n = 16L
    )
  )

  exp_foo <- list(key = quote(foo), value = 1)
  exp_bar <- list(key = quote(bar), value = 2)

  expect_true(dict_it_next(it))
  info1 <- dict_it_info(it)[1:2]

  expect_true(dict_it_next(it))
  info2 <- dict_it_info(it)[1:2]

  if (as_string(info1$key) == "foo") {
    expect_equal(info1, exp_foo)
    expect_equal(info2, exp_bar)
  } else {
    expect_equal(info1, exp_bar)
    expect_equal(info2, exp_foo)
  }

  expect_false(dict_it_next(it))
  expect_false(dict_it_next(it))
})

test_that("can iterate over dict (edge case)", {
  dict <- new_dict(1L, prevent_resize = TRUE)

  dict_put(dict, quote(foo), 1)
  dict_put(dict, quote(bar), 2)

  it <- new_dict_iterator(dict)
  expect_equal(
    dict_it_info(it),
    list(
      key = NULL,
      value = NULL,
      i = 0L,
      n = 1L
    )
  )

  exp_foo <- list(key = quote(foo), value = 1)
  exp_bar <- list(key = quote(bar), value = 2)

  expect_true(dict_it_next(it))
  info1 <- dict_it_info(it)[1:2]

  expect_true(dict_it_next(it))
  info2 <- dict_it_info(it)[1:2]

  if (as_string(info1$key) == "foo") {
    expect_equal(info1, exp_foo)
    expect_equal(info2, exp_bar)
  } else {
    expect_equal(info1, exp_bar)
    expect_equal(info2, exp_foo)
  }

  expect_false(dict_it_next(it))
  expect_false(dict_it_next(it))
})

test_that("can transform dict to list and df-list", {
  dict <- new_dict(10L)

  dict_put(dict, quote(foo), 1)
  dict_put(dict, quote(bar), 2)

  out <- dict_as_df_list(dict)
  foo_first <- as_string(out$key[[1]]) == "foo"

  if (foo_first) {
    exp <- list(
      key = list(quote(foo), quote(bar)),
      value = list(1, 2)
    )
  } else {
    exp <- list(
      key = list(quote(bar), quote(foo)),
      value = list(2, 1)
    )
  }
  expect_equal(out, exp)

  out <- dict_as_list(dict)
  if (foo_first) {
    expect_equal(out, list(1, 2))
  } else {
    expect_equal(out, list(2, 1))
  }
})

test_that("can preserve and unpreserve repeatedly", {
  old <- use_local_precious_list(TRUE)
  on.exit(use_local_precious_list(old))

  x <- env()

  # Need to call rlang_precious_dict() repeatedly because it returns a
  # clone of the dict
  dict <- function() rlang_precious_dict()
  peek_stack <- function() dict_get(dict(), x)
  peek_count <- function() peek_stack()[[1]]

  expect_false(dict_has(dict(), x))

  rlang_preserve(x)
  on.exit(while (dict_has(dict(), x)) {
    rlang_unpreserve(x)
  })

  expect_true(dict_has(dict(), x))

  stack <- peek_stack()
  expect_equal(stack[[1]], 1L)
  expect_equal(stack[[2]], x)

  rlang_preserve(x)
  expect_equal(peek_count(), 2L)

  rlang_unpreserve(x)
  expect_equal(peek_count(), 1L)

  rlang_unpreserve(x)
  expect_false(dict_has(dict(), x))

  expect_error(rlang_unpreserve(x), "Can't unpreserve")
})

test_that("alloc_data_frame() creates data frame", {
  df <- alloc_data_frame(2L, c("a", "b", "c"), c(13L, 14L, 16L))

  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_equal(class(df), "data.frame")
  expect_equal(names(df), c("a", "b", "c"))
  expect_equal(lapply(df, typeof), list(a = "integer", b = "double", c = "character"))
  expect_equal(lapply(df, length), list(a = 2, b = 2, c = 2))

  df <- alloc_data_frame(0L, chr(), int())
  expect_equal(nrow(df), 0)
  expect_equal(ncol(df), 0)
  expect_equal(names(df), chr())

  df <- alloc_data_frame(3L, chr(), int())
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 0)
  expect_equal(names(df), chr())
})

test_that("r_list_compact() compacts lists", {
  expect_equal(list_compact(list()), list())
  expect_equal(list_compact(list(1, 2)), list(1, 2))
  expect_equal(list_compact(list(NULL)), list())
  expect_equal(list_compact(list(NULL, 1)), list(1))
  expect_equal(list_compact(list(1, NULL)), list(1))
  expect_equal(list_compact(list(NULL, 1, NULL, 2, NULL)), list(1, 2))
})

test_that("can grow vectors", {
  x <- 1:3
  out <- vec_resize(x, 5)
  expect_length(out, 5)
  expect_equal(x, 1:3)
  expect_equal(out[1:3], x)

  x <- as.list(1:3)
  out <- vec_resize(x, 5)
  expect_length(out, 5)
  expect_equal(x, as.list(1:3))
  expect_equal(out[1:3], x)
})

test_that("can shrink vectors", {
  x_atomic <- 1:3 + 0L
  out <- vec_resize(x_atomic, 2)
  expect_equal(out, 1:2)

  x_list <- as.list(1:3)
  out <- vec_resize(x_list, 2)
  expect_equal(out, as.list(1:2))

  # Uses truelength to modify in place on recent R
  if (getRversion() >= "3.4.0") {
    expect_equal(x_atomic, 1:2)
    expect_equal(x_list, as.list(1:2))
  }
})

test_that("can grow and shrink dynamic arrays", {
  arr <- new_dyn_array(1, 3)

  expect_equal(
    dyn_info(arr),
    list(
      count = 0,
      capacity = 3,
      growth_factor = 2,
      type = "raw",
      elt_byte_size = 1
    )
  )

  dyn_push_back_bool(arr, FALSE)
  dyn_push_back_bool(arr, TRUE)
  dyn_push_back_bool(arr, TRUE)
  expect_equal(
    dyn_info(arr),
    list(
      count = 3,
      capacity = 3,
      growth_factor = 2,
      type = "raw",
      elt_byte_size = 1
    )
  )

  dyn_push_back_bool(arr, FALSE)
  expect_equal(
    dyn_info(arr)[1:2],
    list(
      count = 4,
      capacity = 6
    )
  )

  dyn_push_back_bool(arr, FALSE)
  dyn_push_back_bool(arr, TRUE)
  expect_equal(
    dyn_info(arr)[1:2],
    list(
      count = 6,
      capacity = 6
    )
  )

  exp <- bytes(0, 1, 1, 0, 0, 1)
  expect_equal(arr[[2]], exp)

  dyn_pop_back(arr)
  expect_equal(
    dyn_info(arr)[1:2],
    list(
      count = 5,
      capacity = 6
    )
  )
  expect_equal(arr[[2]], exp)
})

test_that("can resize dynamic arrays", {
  arr <- new_dyn_array(1, 4)
  dyn_push_back_bool(arr, TRUE)
  dyn_push_back_bool(arr, FALSE)
  dyn_push_back_bool(arr, TRUE)

  dyn_resize(arr, 2L)
  expect_equal(
    dyn_info(arr),
    list(
      count = 2,
      capacity = 2,
      growth_factor = 2,
      type = "raw",
      elt_byte_size = 1
    )
  )
  expect_equal(arr[[2]], bytes(1, 0))

  dyn_resize(arr, 4L)
  expect_equal(
    dyn_info(arr)[1:2],
    list(
      count = 2,
      capacity = 4
    )
  )
  expect_equal(arr[[2]][1:2], bytes(1, 0))
  expect_equal(dyn_unwrap(arr), bytes(1, 0))
})

test_that("dynamic arrays with multiple bytes per elements are resized correctly", {
  arr <- new_dyn_array(4, 4)
  expect_length(arr[[2]], 16)

  dyn_resize(arr, 8L)
  expect_length(arr[[2]], 32)

  arr <- new_dyn_vector("integer", 4)
  expect_length(arr[[2]], 4)

  dyn_resize(arr, 8L)
  expect_length(arr[[2]], 8)
})

test_that("can shrink and grow dynamic atomic vectors", {
  arr <- new_dyn_vector("double", 3)
  expect_equal(
    dyn_info(arr),
    list(
      count = 0,
      capacity = 3,
      growth_factor = 2,
      type = "double",
      elt_byte_size = 8
    )
  )

  dyn_push_back(arr, 1)
  dyn_push_back(arr, 2)
  dyn_push_back(arr, 3)
  expect_equal(
    dyn_info(arr)[1:2],
    list(
      count = 3,
      capacity = 3
    )
  )
  expect_identical(arr[[2]], dbl(1:3))

  dyn_push_back(arr, 4)
  expect_equal(
    dyn_info(arr),
    list(
      count = 4,
      capacity = 6,
      growth_factor = 2,
      type = "double",
      elt_byte_size = 8
    )
  )
  expect_identical(arr[[2]][1:4], dbl(1:4))
  expect_identical(dyn_unwrap(arr), dbl(1:4))
})

test_that("can shrink and grow dynamic barrier vectors", {
  arr <- new_dyn_vector("list", 3)
  expect_equal(
    dyn_info(arr)[1:4],
    list(
      count = 0,
      capacity = 3,
      growth_factor = 2,
      type = "list"
    )
  )

  dyn_push_back(arr, 1)
  dyn_push_back(arr, 2)
  dyn_push_back(arr, 3)
  expect_equal(
    dyn_info(arr)[1:2],
    list(
      count = 3,
      capacity = 3
    )
  )
  expect_identical(arr[[2]], as.list(dbl(1:3)))

  dyn_push_back(arr, 4)
  expect_equal(
    dyn_info(arr)[1:4],
    list(
      count = 4,
      capacity = 6,
      growth_factor = 2,
      type = "list"
    )
  )
  expect_identical(arr[[2]][1:4], as.list(dbl(1:4)))
  expect_identical(dyn_unwrap(arr), as.list(dbl(1:4)))

  expect_equal(dyn_pop_back(arr), 4)
  expect_equal(dyn_pop_back(arr), 3)
  expect_equal(dyn_count(arr), 2)
})

test_that("can get, push, and poke elements", {
  arr <- new_dyn_vector("logical", 3)
  dyn_push_back(arr, TRUE)
  dyn_lgl_push_back(arr, TRUE)
  expect_equal(dyn_lgl_get(arr, 0L), TRUE)
  expect_equal(dyn_lgl_get(arr, 1L), TRUE)
  dyn_lgl_poke(arr, 0L, FALSE)
  expect_equal(dyn_lgl_get(arr, 0L), FALSE)

  arr <- new_dyn_vector("integer", 3)
  dyn_push_back(arr, 1L)
  dyn_int_push_back(arr, 2L)
  expect_equal(dyn_int_get(arr, 0L), 1L)
  expect_equal(dyn_int_get(arr, 1L), 2L)
  dyn_int_poke(arr, 0L, 10L)
  expect_equal(dyn_int_get(arr, 0L), 10L)

  arr <- new_dyn_vector("double", 3)
  dyn_push_back(arr, 1.5)
  dyn_dbl_push_back(arr, 2.5)
  expect_equal(dyn_dbl_get(arr, 0L), 1.5)
  expect_equal(dyn_dbl_get(arr, 1L), 2.5)
  dyn_dbl_poke(arr, 0L, 3.5)
  expect_equal(dyn_dbl_get(arr, 0L), 3.5)

  arr <- new_dyn_vector("complex", 3)
  dyn_push_back(arr, 0i)
  dyn_cpl_push_back(arr, 1i)
  expect_equal(dyn_cpl_get(arr, 0L), 0i)
  expect_equal(dyn_cpl_get(arr, 1L), 1i)
  dyn_cpl_poke(arr, 0L, 2i)
  expect_equal(dyn_cpl_get(arr, 0L), 2i)

  arr <- new_dyn_vector("raw", 3)
  dyn_push_back(arr, as.raw(1))
  dyn_raw_push_back(arr, as.raw(2))
  expect_equal(dyn_raw_get(arr, 0L), as.raw(1))
  expect_equal(dyn_raw_get(arr, 1L), as.raw(2))
  dyn_raw_poke(arr, 0L, as.raw(3))
  expect_equal(dyn_raw_get(arr, 0L), as.raw(3))

  arr <- new_dyn_vector("character", 3)
  foo <- chr_get("foo", 0L)
  bar <- chr_get("bar", 0L)
  dyn_push_back(arr, foo)
  dyn_chr_push_back(arr, bar)
  expect_true(identical(dyn_chr_get(arr, 0L), foo))
  expect_true(identical(dyn_chr_get(arr, 1L), bar))
  baz <- chr_get("bar", 0L)
  dyn_chr_poke(arr, 0L, baz)
  expect_true(identical(dyn_chr_get(arr, 0L), baz))

  arr <- new_dyn_vector("list", 3)
  dyn_push_back(arr, 1:2)
  dyn_list_push_back(arr, 3:4)
  expect_equal(dyn_list_get(arr, 0L), 1:2)
  expect_equal(dyn_list_get(arr, 1L), 3:4)
  dyn_list_poke(arr, 0L, 11:12)
  expect_equal(dyn_list_get(arr, 0L), 11:12)
})

test_that("can create dynamic list-of", {
  lof <- new_dyn_list_of("integer", 5, 2)
  info <- lof_info(lof)

  expect_equal(
    info[c(
      "count",
      "growth_factor",
      "arrays",
      "width",
      "capacity",
      "type",
      "elt_byte_size"
    )],
    list(
      count = 0,
      growth_factor = 2,
      arrays = list(),
      width = 2,
      capacity = 5,
      type = "integer",
      elt_byte_size = 4
    )
  )

  expect_length(lof[[2]], 5 * 2)
})

test_that("can push to dynamic list-of", {
  lof <- new_dyn_list_of("integer", 2, 2)
  info <- lof_info(lof)

  expect_equal(lof_unwrap(lof), list())

  lof_push_back(lof)
  expect_equal(lof_unwrap(lof), list(int()))

  lof_push_back(lof)
  expect_equal(lof_unwrap(lof), list(int(), int()))

  lof_push_back(lof)
  expect_equal(lof_unwrap(lof), list(int(), int(), int()))
})

test_that("internal error is thrown with OOB dyn-lof access", {
  skip_if(!compiled_by_gcc())
  lof <- new_dyn_list_of("integer", 3, 2)
  expect_snapshot({
    err(lof_arr_push_back(lof, 0, 42L), "Location 0 does not exist")
    err(lof_arr_push_back(lof, 10, 42L), "Location 10 does not exist")
  })
})

test_that("can push to arrays in dynamic list-of", {
  lof <- new_dyn_list_of("integer", 3, 2)
  lof_push_back(lof)
  lof_push_back(lof)
  lof_push_back(lof)
  lof_push_back(lof)

  expect_error(lof_arr_push_back(lof, 0, 42), "type double")

  lof_arr_push_back(lof, 0, 42L)
  expect_equal(
    lof_unwrap(lof),
    list(42L, int(), int(), int())
  )

  lof_arr_push_back(lof, 3, 42L)
  expect_equal(
    lof_unwrap(lof),
    list(42L, int(), int(), 42L)
  )

  # Trigger resizes of the reserve
  lof_arr_push_back(lof, 0, 43L)
  lof_arr_push_back(lof, 0, 44L)
  expect_equal(
    lof_unwrap(lof),
    list(42:44, int(), int(), 42L)
  )

  lof_arr_push_back(lof, 2, 42L)
  lof_arr_push_back(lof, 2, 43L)
  lof_arr_push_back(lof, 2, 44L)
  expect_equal(
    lof_unwrap(lof),
    list(42:44, int(), 42:44, 42L)
  )

  # Trigger resize in the moved array
  lof_arr_push_back(lof, 3, 43L)
  lof_arr_push_back(lof, 3, 44L)
  expect_equal(
    lof_unwrap(lof),
    list(42:44, int(), 42:44, 42:44)
  )
})

test_that("sexp iterator visits in full order", {
  skip_if_not(has_private_accessors())

  it_dirs <- function(snapshot) {
    dirs <- sapply(snapshot, `[[`, "dir")
    dirs <- table(dirs)
    nms <- names(dirs)
    dim(dirs) <- NULL
    set_names(dirs, nms)
  }
  expect_symmetric_dirs <- function(s) {
    dirs <- it_dirs(s)
    expect_equal(s[["incoming"]], s[["outgoing"]])
  }
  expect_symmetric_dirs(sexp_iterate(list(1), list))
  expect_symmetric_dirs(sexp_iterate(list(1, 2), list))
  expect_symmetric_dirs(sexp_iterate(list(1, list()), list))
  expect_symmetric_dirs(sexp_iterate(list(1, list(2)), list))
  expect_symmetric_dirs(sexp_iterate(list(emptyenv(), emptyenv()), list))
})

test_that("addresses have hexadecimal prefix `0x` (#1135)", {
  expect_equal(
    substring(obj_address(NULL), 1, 2),
    "0x"
  )
})

test_that("can re-encode a character vector of various encodings (r-lib/vctrs#553)", {
  x <- unlist(test_encodings(), use.names = FALSE)
  results <- r_obj_encode_utf8(x)
  expect_utf8_encoded(results)
})

test_that("re-encodes all encodings to UTF-8", {
  for (enc in test_encodings()) {
    expect_utf8_encoded(r_obj_encode_utf8(enc))
  }
})

test_that("can re-encode a list containing character vectors with different encodings", {
  results <- r_obj_encode_utf8(test_encodings())
  results <- unlist(results)
  expect_utf8_encoded(results)
})

test_that("re-encoding fails purposefully with any bytes", {
  bytes <- rawToChar(as.raw(0xdc))
  Encoding(bytes) <- "bytes"

  expect_snapshot(
    (expect_error(r_obj_encode_utf8(bytes)))
  )

  for (enc in test_encodings()) {
    expect_snapshot(
      (expect_error(r_obj_encode_utf8(c(enc, bytes))))
    )
  }
})

test_that("attributes are kept when re-encoding (r-lib/vctrs#599)", {
  encs <- test_encodings()

  x <- c(encs$utf8, encs$latin1)
  x <- structure(x, names = c("a", "b"), extra = 1)

  expect_identical(attributes(r_obj_encode_utf8(x)), attributes(x))
})

test_that("re-encoding is robust against scalar types contained in lists (r-lib/vctrs#633)", {
  x <- list(a = z ~ y, b = z ~ z)
  expect_identical(r_obj_encode_utf8(x), x)
})

test_that("re-encoding can still occur even if a scalar type is in a list", {
  x <- list(a = z ~ y, b = test_encodings()$latin1)
  expect_utf8_encoded(r_obj_encode_utf8(x)$b)
})

test_that("re-encoding occurs inside scalars contained in a list", {
  encs <- test_encodings()

  x <- list(
    structure(list(x = encs$latin1), class = "scalar_list")
  )

  result <- r_obj_encode_utf8(x)

  expect_utf8_encoded(result[[1]]$x)
})

test_that("re-encoding treats data frames elements of lists as lists (r-lib/vctrs#1233)", {
  encs <- test_encodings()
  a <- c(encs$utf8, encs$latin1)

  df <- data.frame(a = a, b = 1:2, stringsAsFactors = FALSE)
  x <- list(df)

  result <- r_obj_encode_utf8(x)

  expect_utf8_encoded(result[[1]]$a)
})

test_that("attributes are re-encoded", {
  utf8 <- test_encodings()$utf8
  latin1 <- test_encodings()$latin1

  a <- structure(1, enc = utf8)
  b <- structure(1, enc = latin1)
  c <- structure(1, enc1 = utf8, enc2 = list(latin1), enc3 = latin1)
  x <- list(a, b, c)

  result <- r_obj_encode_utf8(x)

  a_enc <- attr(result[[1]], "enc")
  b_enc <- attr(result[[2]], "enc")
  c_enc1 <- attr(result[[3]], "enc1")
  c_enc2 <- attr(result[[3]], "enc2")[[1]]
  c_enc3 <- attr(result[[3]], "enc3")

  expect_utf8_encoded(a_enc)
  expect_utf8_encoded(b_enc)
  expect_utf8_encoded(c_enc1)
  expect_utf8_encoded(c_enc2)
  expect_utf8_encoded(c_enc3)
})

test_that("attributes are re-encoded recursively", {
  utf8 <- test_encodings()$utf8
  latin1 <- test_encodings()$latin1

  nested <- structure(1, latin1 = latin1)
  x <- structure(2, nested = nested, foo = 1, latin1 = latin1)

  result <- r_obj_encode_utf8(x)
  attrib <- attributes(result)
  attrib_nested <- attributes(attrib$nested)

  expect_utf8_encoded(attrib$latin1)
  expect_utf8_encoded(attrib_nested$latin1)
})

test_that("NAs aren't re-encoded to 'NA' (r-lib/vctrs#1291)", {
  utf8 <- c(NA, test_encodings()$utf8)
  latin1 <- c(NA, test_encodings()$latin1)

  result1 <- r_obj_encode_utf8(utf8)
  result2 <- r_obj_encode_utf8(latin1)

  expect_identical(result1[[1]], NA_character_)
  expect_identical(result2[[1]], NA_character_)

  expect_utf8_encoded(result1[[2]])
  expect_utf8_encoded(result2[[2]])
})

local({
  df <- c_tests()
  for (i in seq_len(nrow(df))) {
    desc <- df[[1]][[i]]
    ptr <- df[[2]][[i]]

    test_that(desc, {
      expect_true(run_c_test(ptr))
    })
  }
})

test_that("r_stop_internal() mentions expected namespace", {
  fn <- function() {
    .Call(get("ffi_test_stop_internal", envir = asNamespace("rlang")), "Message.")
  }

  environment(fn) <- ns_env("base")
  expect_error(fn(), "detected in the base package")

  environment(fn) <- ns_env("utils")
  expect_error(fn(), "detected in the utils package")
})
