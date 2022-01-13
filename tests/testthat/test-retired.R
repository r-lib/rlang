local_lifecycle_silence()


# Deprecated in rlang 0.4.0 ------------------------------------------

test_that("type_of() returns correct type", {
  expect_identical(type_of("foo"), "string")
  expect_identical(type_of(letters), "character")
  expect_identical(type_of(base::`$`), "primitive")
  expect_identical(type_of(base::list), "primitive")
  expect_identical(type_of(base::eval), "closure")
  expect_identical(type_of(~foo), "formula")
  expect_identical(type_of(quo(foo)), "formula")
  expect_identical(type_of(quote(foo())), "language")
})

test_that("Unicode escapes are converted to UTF8 characters in env_names()", {
  with_non_utf8_locale({
    env <- child_env(empty_env())
    env_bind(env, !!get_alien_lang_string() := NULL)
    nms <- env_names(env)
    expect_identical(nms, get_alien_lang_string())
  })
})

test_that("no method dispatch", {
  as.logical.foo <- function(x) "wrong"
  expect_identical(as_integer(structure(TRUE, class = "foo")), 1L)

  as.list.foo <- function(x) "wrong"
  expect_identical(as_list(structure(1:10, class = "foo")), as.list(1:10))
})

test_that("input is left intact", {
  x <- structure(TRUE, class = "foo")
  y <- as_integer(x)
  expect_identical(x, structure(TRUE, class = "foo"))
})

test_that("as_list() zaps attributes", {
  expect_identical(as_list(structure(list(), class = "foo")), list())
})

test_that("as_list() only coerces vector or dictionary types", {
  expect_identical(as_list(1:3), list(1L, 2L, 3L))
  expect_error(as_list(quote(symbol)), "a symbol to")
})

test_that("as_list() bypasses environment method and leaves input intact", {
  as.list.foo <- function(x) "wrong"
  x <- structure(child_env(NULL), class = "foo")
  y <- as_list(x)

  expect_s3_class(x, "foo")
  expect_identical(y, set_names(list(), character(0)))
})

test_that("as_integer() and as_logical() require integerish input", {
  expect_error(as_integer(1.5), "a fractional double vector to")
  expect_error(as_logical(1.5), "a fractional double vector to")
})

test_that("names are preserved", {
  nms <- as.character(1:3)
  x <- set_names(1:3, nms)
  expect_identical(names(as_double(x)), nms)
  expect_identical(names(as_list(x)), nms)
})

test_that("as_character() support logical NA", {
  expect_identical(as_character(NA), na_chr)
  expect_identical(as_character(lgl(NA, NA)), c(na_chr, na_chr))
})

test_that("can convert strings (#138)", {
  expect_identical(as_character("a"), "a")
  expect_identical(as_list("a"), list("a"))
})


# --------------------------------------------------------------------

test_that("set_attrs() fails with uncopyable types", {
  expect_error(set_attrs(env(), foo = "bar"), "is uncopyable")
})

test_that("set_attrs() called with NULL zaps attributes", {
  obj <- set_attrs(letters, foo = "bar")
  expect_identical(set_attrs(obj, NULL), letters)
})

test_that("set_attrs() does not zap old attributes", {
  obj <- set_attrs(letters, foo = "bar")
  obj <- set_attrs(obj, baz = "bam")
  expect_named(attributes(obj), c("foo", "baz"))
})

test_that("invoke() buries arguments", {
  expect_identical(invoke(call_inspect, 1:2, 3L), quote(.fn(`1`, `2`, `3`)))
  expect_identical(invoke("call_inspect", 1:2, 3L), quote(call_inspect(`1`, `2`, `3`)))
  expect_identical(invoke(call_inspect, 1:2, 3L, .bury = c("foo", "bar")), quote(foo(`bar1`, `bar2`, `bar3`)))
  expect_identical(invoke(call_inspect, 1:2, 3L, .bury = NULL), as.call(list(call_inspect, 1L, 2L, 3L)))
})

test_that("invoke() can be called without arguments", {
  expect_identical(invoke("list"), list())
  expect_identical(invoke(list), list())
})

test_that("quo_expr() still works", {
  x <- quo(foo(!!quo(bar), !!local(quo(baz))))
  expect_identical(quo_expr(x), quo_squash(x))
})

test_that("call_fn() extracts function", {
  expect_identical(call_fn(~matrix()), matrix)
})

test_that("call_fn() looks up function in `env`", {
  env <- local({
    fn <- function() "foo"
    current_env()
  })
  expect_identical(call_fn(quote(fn()), env = env), env$fn)
})

test_that("with_handlers() establishes inplace and exiting handlers", {
  handlers <- list(
    error = function(c) "caught error",
    message = function(c) "caught message",
    warning = calling(function(c) "warning"),
    foobar = calling(function(c) cat("foobar"))
  )

  expect_equal(with_handlers(identity(letters), !!!handlers), identity(letters))
  expect_equal(with_handlers(stop(letters), !!!handlers), "caught error")
  expect_equal(with_handlers(message(letters), !!!handlers), "caught message")
  expect_warning(expect_equal(with_handlers({ warning("warn!"); letters }, !!!handlers), identity(letters)), "warn!")
  expect_output(expect_equal(with_handlers({ signal("", "foobar"); letters }, !!!handlers), identity(letters)), "foobar")
})

test_that("with_handlers() propagates visibility", {
  expect_visible(with_handlers(list(invisible(1))))
  expect_invisible(with_handlers(invisible(1)))
})

test_that("bare functions are treated as exiting handlers", {
  expect_identical(with_handlers(abort("foo"), error = function(cnd) "caught"), "caught")
})

test_that("with_handlers() supports formula shortcut for lambdas", {
  err <- with_handlers(abort("foo", "bar"), error = ~.x)
  expect_true(inherits(err, "bar"))
})

test_that("can muffle conditions", {
  expect_no_message(
    expect_identical(with_handlers({ message(""); "foo" }, message = calling(cnd_muffle)), "foo")
  )
  expect_no_warning(
    expect_identical(with_handlers({ warning(""); "foo" }, warning = calling(cnd_muffle)), "foo")
  )
  cnd_expect_muffle <- calling(function(cnd) {
    expect_s3_class(findRestart("rlang_muffle"), "restart")
    cnd_muffle(cnd)
  })
  expect_identical(with_handlers({ signal("", "cnd"); "foo" }, cnd = cnd_expect_muffle), "foo")
})
