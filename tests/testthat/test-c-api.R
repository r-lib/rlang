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

test_that("r_which_operator() returns correct tokens", {
  expect_identical(which_operator(quote(foo())), "")
  expect_identical(which_operator(""), "")

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


  # We store the library as a zip to avoid VCS noise
  utils::unzip(zip_file, exdir = file.path(pkg_path, "src"))

  # For maintenance
  regenerate_zip <- function() {
    location <- file.path("..", "..", "src")
    old <- setwd(location)
    on.exit(setwd(old))

    lib_files <- c("lib.c", "lib")
    file.remove(zip_file)
    utils::zip(zip_file, lib_files)
  }

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
  c_env_unbind <- function(env, names, inherits = FALSE) {
    invisible(.Call(rlang_env_unbind, env, names, inherits))
  }

  env <- env(a = 1L)
  c_env_unbind(env, "a")
  expect_false(env_has(env, "a"))

  env <- env(a = 1L)
  child <- child_env(env)
  expect_warning(c_env_unbind(child, "a"), "not found")
  c_env_unbind(child, "a", inherits = TRUE)
  expect_false(env_has(env, "a"))
})
