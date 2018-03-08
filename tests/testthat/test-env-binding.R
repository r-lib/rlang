context("env-binding")

test_that("promises are created", {
  env <- child_env(NULL)

  env_bind_exprs(env, foo = bar <- "bar")
  expect_false(env_has(current_env(), "bar"))

  force(env$foo)
  expect_true(env_has(current_env(), "bar"))

  env_bind_exprs(env, stop = stop("forced"))
  expect_error(env$stop, "forced")
})

test_that("env_bind_fns() creates active bindings", {
  env <- env_bind_fns(env(), a = function() "foo")
  expect_identical(env$a, "foo")
  expect_identical(env$a, "foo")
})

test_that("env_poke() returns previous value", {
  env <- env(env(empty_env(), bar = "bar"))
  expect_identical(env_poke(env, "foo", "foo"), missing_arg())
  expect_identical(env_poke(env, "foo", "FOO"), "foo")
  expect_identical(env_poke(env, "bar", "foo", inherit = TRUE), "bar")
})

test_that("env_poke() creates binding if `create` is TRUE", {
  env <- new_environment()
  env_poke(env, "foo", "foo")
  expect_identical(env_get(env, "foo"), "foo")

  expect_error(env_poke(env, "bar", "BAR", create = FALSE), "Can't find existing binding")
  env_poke(env, "foo", "FOO", create = FALSE)
  expect_identical(env_get(env, "foo"), "FOO")
})

test_that("env_poke() inherits from parents if `inherit` is TRUE", {
  env <- child_env(new_environment(), foo = "foo")
  env <- child_env(env)

  env_has(env, "foo")
  env_has(env, "foo", TRUE)

  env_poke(env, "foo", "FOO", inherit = TRUE, create = FALSE)
  expect_identical(env_get(env_parent(env), "foo", inherit = FALSE), "FOO")

  expect_error(env_poke(env, "bar", "bar", inherit = TRUE, create = FALSE), "Can't find existing binding")
  expect_error(env_poke(env, "bar", "bar", inherit = TRUE), "Can't find existing binding")

  env_poke(env, "bar", "bar", inherit = TRUE, create = TRUE)
  expect_identical(env_get(env, "bar"), "bar")
})

test_that("env_get_list() retrieves multiple bindings", {
  env <- env(foo = 1L, bar = 2L)
  expect_identical(env_get_list(env, c("foo", "bar")), list(foo = 1L, bar =2L))

  baz <- 0L
  expect_error(env_get_list(env, "baz"), "'baz' not found")
  expect_identical(env_get_list(env, c("foo", "baz"), inherit = TRUE), list(foo = 1L, baz =0L))
})

test_that("scoped_bindings binds temporarily", {
  env <- env(foo = "foo", bar = "bar")

  local({
    old <- scoped_bindings(.env = env,
      foo = "FOO",
      bar = "BAR",
      baz = "BAZ"
    )
    expect_identical(old, list(foo = "foo", bar = "bar"))
    temp_bindings <- env_get_list(env, c("foo", "bar", "baz"))
    expect_identical(temp_bindings, list(foo = "FOO", bar = "BAR", baz = "BAZ"))
  })

  bindings <- env_get_list(env, c("foo", "bar"))
  expect_identical(bindings, list(foo = "foo", bar = "bar"))
  expect_false(env_has(env, "baz"))
})

test_that("with_bindings() evaluates with temporary bindings", {
  foo <- "foo"
  baz <- "baz"
  expect_identical(with_bindings(paste(foo, baz), foo = "FOO"), "FOO baz")
  expect_identical(foo, "foo")
})

test_that("env_unbind() with `inherits = TRUE` wipes out all bindings", {
  bindings <- list(`_foo` = "foo", `_bar` = "bar")
  env_bind(global_env(), !!! bindings)
  env <- child_env(global_env(), !!! bindings)

  env_unbind(env, "_foo", inherit = TRUE)
  expect_false(all(env_has(env, names(bindings))))
  expect_false(all(env_has(global_env(), names(bindings))))
})

test_that("env_bind() requires named elements", {
  expect_error(env_bind(env(), 1), "all arguments must be named")
  expect_error(env_bind(env(), !!! list(1)), "all arguments must be named")
})

test_that("env_bind() requires uniquely named elements", {
  expect_error(env_bind(env(), a = 1, a = 2), "some arguments have the same name")
})
test_that("env_bind() works with empty unnamed lists", {
  expect_no_error(env_bind(env()))
  expect_no_error(env_bind(env(), !!! list()))
})

test_that("env_names() unserialises unicode", {
  env <- env(`<U+5E78><U+798F>` = "foo")
  expect_identical(env_names(env), "\u5E78\u798F")
})

test_that("env_has() returns a named vector", {
  expect_identical(env_has(env(a = TRUE), c("a", "b", "c")), c(a = TRUE, b = FALSE, c = FALSE))
})

test_that("env_bind_impl() fails if data is not a vector", {
  expect_error(env_bind_impl(env(), env()), "must be a vector")
})

test_that("env_unbind() doesn't warn if binding doesn't exist (#177)", {
  expect_no_warning(env_unbind(env(), c("foo", "bar")))
})

test_that("env_get() and env_get_list() accept default value", {
  env <- env(a = 1)

  expect_error(env_get(env, "b"), "not found")
  expect_error(env_get_list(env, "b"), "not found")

  expect_identical(env_get(env, "b", default = "foo"), "foo")
  expect_identical(env_get_list(env, c("a", "b"), default = "foo"), list(a = 1, b = "foo"))
})

test_that("env_bind_fns() uses as_function()", {
  env_bind_fns(current_env(), foo = ~2 + 3)
  expect_identical(foo, 5)
})

test_that("env_bind_fns() and env_bind_exprs() redefine bindings", {
  env <- env(a = 1, b = 2)
  env_bind_fns(env, a = ~"foo")
  env_bind_exprs(env, b = "bar")
  expect_identical(c(env$a, env$b), c("foo", "bar"))
})

test_that("binding predicates detect special bindings", {
  env <- env()
  env_bind_fns(env, a = ~toupper("foo"))
  env_bind_exprs(env, b = toupper("foo"))
  env_bind(env, c = toupper("foo"), d = "irrelevant")

  expect_identical(env_binding_are_active(env, c("a", "b", "c")), c(a = TRUE, b = FALSE, c = FALSE))
  expect_identical(env_binding_are_promise(env, c("a", "b", "c")), c(a = FALSE, b = TRUE, c = FALSE))

  force(env$b)
  expect_identical(env_binding_are_promise(env, c("a", "b", "c")), c(a = FALSE, b = FALSE, c = FALSE))
})

test_that("applies predicates to all bindings by default", {
  env <- env()
  env_bind_fns(env, a = ~toupper("foo"))
  env_bind_exprs(env, b = toupper("foo"))
  env_bind(env, c = toupper("foo"))
  expect_identical(env_binding_are_active(env), c(a = TRUE, b = FALSE, c = FALSE))
  expect_identical(env_binding_are_promise(env), c(a = FALSE, b = TRUE, c = FALSE))
})

test_that("env_binding_are_active() doesn't force promises", {
  env <- env_bind_exprs(env(), foo = stop("kaboom"))
  expect_no_error(env_binding_are_active(env))
  expect_identical(env_binding_are_promise(env), lgl(foo = TRUE))
  expect_identical(env_binding_are_promise(env), lgl(foo = TRUE))
})

test_that("env_binding_type_sum() detects types", {
  env <- env()
  env_bind_fns(env, a = ~"foo")
  env_bind_exprs(env, b = identity("foo"))
  env_bind(env,
    c = "foo",
    d = 1L,
    e = 2
  )

  expect_error(env_binding_type_sum(env, 1L), "must be a character vector")

  types <- c(a = "active", b = "promise", c = "chr", d = "int", e = "dbl")
  expect_identical(env_binding_type_sum(env), types)
})

test_that("can lock and unlock bindings", {
  env <- env(a = "A", b = "B")
  expect_identical(env_binding_are_locked(env), lgl(a = FALSE, b = FALSE))

  expect_identical(env_binding_lock(env, "a"), lgl(a = FALSE))

  locked <- env_binding_are_locked(env)
  expect_identical(locked, lgl(a = TRUE, b = FALSE))

  expect_identical(env_binding_unlock(env), locked)
  expect_identical(env_binding_are_locked(env), lgl(a = FALSE, b = FALSE))
})
