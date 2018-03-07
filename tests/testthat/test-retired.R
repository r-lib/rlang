context("retired")

test_that("parse_quosure() forwards to parse_quo()", {
  env <- env()
  expect_warning(expect_identical(parse_quosure("foo", env), parse_quo("foo", env)), "soft-deprecated")
  expect_warning(expect_identical(parse_quosures("foo; bar", env), parse_quos("foo; bar", env)), "soft-deprecated")
})

test_that("quo_expr() forwards to quo_squash()", {
  quo <- quo(list(!!quo(foo)))
  expect_identical(quo_expr(quo), quo_squash(quo))
})

test_that("lang() forwards to call2() and is_lang() to is_call()", {
  lang <- lang("foo", !!! list(1, 2), .ns = "bar")
  call <- call2("foo", !!! list(1, 2), .ns = "bar")
  expect_identical(lang, call)
  expect_true(is_lang(lang, "foo", 2, "bar"))
  expect_false(is_unary_lang(lang, "foo", "bar"))
  expect_true(is_binary_lang(lang, "foo", "bar"))
})

test_that("new_language() forwards to new_call()", {
  expect_identical(
    new_language(quote(foo), pairlist("bar")),
    new_call(quote(foo), pairlist("bar"))
  )
})

test_that("lang_modify() forwards to call_modify()", {
  fn <- function(foo = "bar") NULL
  call <- quote(fn(f = "foo"))
  expect_identical(
    lang_modify(call, baz = "bam", .standardise = TRUE),
    call_modify(call, baz = "bam", .standardise = TRUE)
  )
})

test_that("lang_standardise() forwards to call_standardise()", {
  fn <- function(foo = "bar") NULL
  call <- quote(fn(f = "foo"))
  expect_identical(
    lang_standardise(call),
    call_standardise(call)
  )
})

test_that("`lang_` accessors forward to `call_` accessors", {
  fn <- function(foo = "bar") NULL
  call <- quote(fn(f = "foo"))
  expect_identical(lang_fn(call), fn)
  expect_identical(lang_name(call), "fn")
  expect_identical(lang_args(call), list(f = "foo"))
  expect_identical(lang_args_names(call), "f")
})

test_that("lang_tail() still works", {
  expect_identical(
    pairlist(sym("a")),
    lang_tail(expr(foo(a)))
  )
})

test_that("lang_head() still works", {
  expect_identical(
    lang_head(expr(foo(a))),
    expr(foo)
  )
})

test_that("as_overscope() forwards to as_data_mask()", {
  quo <- quo(foo)
  expect_equal(as_overscope(quo, mtcars), as_data_mask(mtcars, quo_get_env(quo)))
})

test_that("overscope functions forward to mask functions", {
  top <- env()
  bottom <- child_env(top, foo = "bar")
  mask <- new_overscope(bottom, top)
  expect_true(env_has(mask, ".__tidyeval_data_mask__."))

  expect_identical(eval_tidy_(quote(foo), bottom, top), "bar")

  overscope_clean(mask)
  expect_false(env_has(env_parent(mask), "foo"))

  mask <- as_data_mask(mtcars)
  x <- 10
  expect_identical(overscope_eval_next(mask, quote(cyl * x), current_env()), mtcars$cyl * x)
  expect_identical(overscope_eval_next(mask, quote(am * x), current_env()), mtcars$am * x)
})

test_that("as_dictionary() forwards to as_data_pronoun()", {
  dict <- as_dictionary(mtcars, "Column `%s` not found in `.data`", TRUE)
  expect_identical(dict, as_data_pronoun(mtcars))

  dict <- as_dictionary(list2env(mtcars), "Column `%s` not found in `.data`", TRUE)
  expect_equal(dict, as_data_pronoun(list2env(mtcars)))

  expect_true(is_dictionary(dict))
})

test_that("as_env() forwards to as_environment()", {
  x <- as_env(mtcars, base_env())
  y <- as_environment(mtcars, base_env())
  expect_equal(x, y)
  expect_identical(env_parent(x), env_parent(y))
})

test_that("is_expr() forwards to is_expression()", {
  expect_true(is_expr(1L))
  expect_false(is_expr(1:2))
})

test_that("is_quosureish() and as_quosureish() still work", {
  expect_warning(expect_true(is_quosureish(~foo)), "deprecated")
  expect_warning(expect_false(is_quosureish(~foo, scoped = FALSE)), "deprecated")
  expect_warning(expect_identical(as_quosureish(quote(foo)), quo(foo)), "deprecated")
})

test_that("new_cnd() and cnd_ functions forward to cnd() and _cnd functions()", {
  expect_warning(expect_identical(new_cnd("foo"), cnd("foo")), "renamed")
  expect_warning(expect_identical(cnd_warning("foo"), warning_cnd("foo")), "renamed")
  expect_warning(expect_identical(cnd_error("foo"), error_cnd("foo")), "renamed")
  expect_warning(expect_identical(cnd_message("foo"), message_cnd("foo")), "renamed")
})

test_that("node() still works", {
  expect_identical(node(1, NULL), new_node(1, NULL))
})
