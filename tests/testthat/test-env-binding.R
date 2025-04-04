test_that("promises are created", {
  env <- child_env(NULL)

  env_bind_lazy(env, foo = bar <- "bar")
  expect_false(env_has(current_env(), "bar"))

  force(env$foo)
  expect_true(env_has(current_env(), "bar"))

  env_bind_lazy(env, stop = stop("forced"))
  expect_error(env$stop, "forced")
})

test_that("env_bind_active() creates active bindings", {
  env <- env()
  env_bind_active(env, a = function() "foo")
  expect_identical(env$a, "foo")
  expect_identical(env$a, "foo")
})

test_that("env_poke() returns previous value", {
  env <- env(env(empty_env(), bar = "bar"))
  expect_identical(env_poke(env, "foo", "foo"), zap())
  expect_identical(env_poke(env, "foo", "FOO"), "foo")
  expect_identical(env_poke(env, "bar", "foo", inherit = TRUE), "bar")
})

test_that("env_poke() creates binding if `create` is TRUE", {
  env <- new_environment()
  env_poke(env, "foo", "foo")
  expect_identical(env_get(env, "foo"), "foo")

  expect_error(
    env_poke(env, "bar", "BAR", create = FALSE),
    "Can't find existing binding"
  )
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

  expect_error(
    env_poke(env, "bar", "bar", inherit = TRUE, create = FALSE),
    "Can't find existing binding"
  )
  expect_error(
    env_poke(env, "bar", "bar", inherit = TRUE),
    "Can't find existing binding"
  )

  env_poke(env, "bar", "bar", inherit = TRUE, create = TRUE)
  expect_identical(env_get(env, "bar"), "bar")
})

test_that("env_get() evaluates promises and active bindings", {
  e <- env()
  env_bind_lazy(e, x = 1)
  env_bind_active(e, y = function() 2)

  expect_equal(env_get(e, "x"), 1)
  expect_equal(env_get(e, "y"), 2)
})

test_that("env_get_list() retrieves multiple bindings", {
  env <- env(foo = 1L, bar = 2L)
  expect_identical(env_get_list(env, c("foo", "bar")), list(foo = 1L, bar = 2L))

  baz <- 0L
  expect_error(env_get_list(env, "baz"), "Can't find")
  expect_identical(
    env_get_list(env, c("foo", "baz"), inherit = TRUE),
    list(foo = 1L, baz = 0L)
  )
})

test_that("local_bindings binds temporarily", {
  env <- env(foo = "foo", bar = "bar")

  local({
    old <- local_bindings(.env = env, foo = "FOO", bar = "BAR", baz = "BAZ")
    expect_identical(old, list3(foo = "foo", bar = "bar", baz = zap()))
    temp_bindings <- env_get_list(env, c("foo", "bar", "baz"))
    expect_identical(temp_bindings, list(foo = "FOO", bar = "BAR", baz = "BAZ"))
  })

  bindings <- env_get_list(env, c("foo", "bar"))
  expect_identical(bindings, list(foo = "foo", bar = "bar"))
  expect_false(env_has(env, "baz"))
})

test_that("local_bindings() restores in correct order", {
  foo <- "start"

  local({
    local_bindings(foo = "foo")
    expect_identical(foo, "foo")

    local_bindings(foo = "bar")
    expect_identical(foo, "bar")
  })

  expect_identical(foo, "start")
})

test_that("with_bindings() evaluates with temporary bindings", {
  foo <- "foo"
  baz <- "baz"
  expect_identical(with_bindings(paste(foo, baz), foo = "FOO"), "FOO baz")
  expect_identical(foo, "foo")
})

test_that("env_unbind() with `inherits = TRUE` only removes first match", {
  env <- env(foo = "foo")
  child <- env(env, foo = "foo")

  env_unbind(child, "foo", inherit = TRUE)
  expect_false(env_has(child, "foo"))
  expect_true(env_has(env, "foo"))
})

test_that("env_bind() requires named elements", {
  expect_error(env_bind(env(), 1), "some elements are not named")
  expect_error(env_bind(env(), !!!list(1)), "some elements are not named")
})

test_that("env_bind() works with empty unnamed lists", {
  expect_no_error(env_bind(env()))
  expect_no_error(env_bind(env(), !!!list()))
})

test_that("env_names() unserialises unicode", {
  env <- env(`<U+5E78><U+798F>` = "foo")
  expect_identical(env_names(env), "\u5E78\u798F")
})

test_that("env_has() returns a named vector", {
  expect_identical(
    env_has(env(a = TRUE), c("a", "b", "c")),
    c(a = TRUE, b = FALSE, c = FALSE)
  )
})

test_that("env_unbind() doesn't warn if binding doesn't exist (#177)", {
  expect_no_warning(env_unbind(env(), c("foo", "bar")))
})

test_that("env_get() and env_get_list() accept default value", {
  env <- env(a = 1)

  expect_error(env_get(env, "b"), "Can't find")
  expect_error(env_get_list(env, "b"), "Can't find")

  expect_identical(env_get(env, "b", default = "foo"), "foo")
  expect_identical(
    env_get_list(env, c("a", "b"), default = "foo"),
    list(a = 1, b = "foo")
  )
})

test_that("env_get() without default fails", {
  expect_snapshot(error = TRUE, cnd_class = TRUE, {
    env_get(env(), "foobar")
    env_get_list(env(), "foobar")
  })

  fn <- function(env, default) env_get(env, "_foobar", default = default)
  expect_error(fn(env()), "Can't find")
})

test_that("env_get() evaluates `default` lazily", {
  expect_equal(env_get(env(a = 1), "a", default = stop("tilt")), 1)
})

test_that("env_bind_active() uses as_function()", {
  env_bind_active(current_env(), foo = ~ 2 + 3)
  expect_identical(foo, 5)
})

test_that("env_bind_active() and env_bind_lazy() redefine bindings", {
  env <- env(a = 1, b = 2)
  env_bind_active(env, a = ~"foo")
  env_bind_lazy(env, b = "bar")
  expect_identical(c(env$a, env$b), c("foo", "bar"))
})

test_that("binding predicates detect special bindings", {
  env <- env()
  env_bind_active(env, a = ~ toupper("foo"))
  env_bind_lazy(env, b = toupper("foo"))
  env_bind(env, c = toupper("foo"), d = "irrelevant")

  expect_identical(
    env_binding_are_active(env, c("a", "b", "c")),
    c(a = TRUE, b = FALSE, c = FALSE)
  )
  expect_identical(
    env_binding_are_lazy(env, c("a", "b", "c")),
    c(a = FALSE, b = TRUE, c = FALSE)
  )

  force(env$b)
  expect_identical(
    env_binding_are_lazy(env, c("a", "b", "c")),
    c(a = FALSE, b = FALSE, c = FALSE)
  )

  env <- env(a = 1, b = 2)
  expect_identical(env_binding_are_active(env), c(a = FALSE, b = FALSE))
  expect_identical(env_binding_are_lazy(env), c(a = FALSE, b = FALSE))
})

test_that("applies predicates to all bindings by default", {
  env <- env()
  env_bind_active(env, a = ~ toupper("foo"))
  env_bind_lazy(env, b = toupper("foo"))
  env_bind(env, c = toupper("foo"))
  expect_identical(
    env_binding_are_active(env),
    c(a = TRUE, b = FALSE, c = FALSE)
  )
  expect_identical(env_binding_are_lazy(env), c(a = FALSE, b = TRUE, c = FALSE))
})

test_that("env_binding_are_active() doesn't force promises", {
  env <- env()
  env_bind_lazy(env, foo = stop("kaboom"))
  expect_no_error(env_binding_are_active(env))
  expect_identical(env_binding_are_lazy(env), lgl(foo = TRUE))
  expect_identical(env_binding_are_lazy(env), lgl(foo = TRUE))
})

test_that("env_binding_are_active() doesn't trigger active bindings (#1376)", {
  env <- env()
  env_bind_active(env, foo = ~ stop("kaboom"))
  expect_no_error(env_binding_are_active(env))
  expect_identical(env_binding_are_active(env), lgl(foo = TRUE))
  expect_identical(env_binding_are_lazy(env), lgl(foo = FALSE))
})

test_that("env_binding_type_sum() detects types", {
  env <- env()
  env_bind_active(env, a = ~"foo")
  env_bind_lazy(env, b = identity("foo"))
  env_bind(env, c = "foo", d = 1L, e = 2)

  expect_error(env_binding_type_sum(env, 1L), "must be a character vector")

  types <- c(a = "active", b = "lazy", c = "chr", d = "int", e = "dbl")
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

test_that("can pluck missing arg from environment", {
  env <- env(x = missing_arg())
  expect_identical(env_get(env, "x"), missing_arg())
  expect_identical(env_get_list(env, "x"), list(x = missing_arg()))

  skip("Failing")
  child <- env(env)
  env_get(child, "x", inherit = TRUE)
})

test_that("can call local_bindings() and with_bindings() without arguments", {
  expect_no_error(local_bindings())
  expect_no_error(with_bindings("foo"))
})

test_that("can bind missing args", {
  e <- env()

  expect_no_error(env_bind(e, foo = ))
  expect_identical(e$foo, missing_arg())

  args <- list(bar = expr(), baz = expr())
  expect_no_error(env_bind(e, !!!args))
  expect_identical(env_get_list(e, c("bar", "baz")), args)
})

test_that("can remove bindings by supplying zaps", {
  empty <- env()
  expect_no_error(env_bind(empty, foo = zap()))

  env <- env(foo = "foo", bar = "bar")
  env_bind(env, foo = zap(), bar = zap())
  expect_identical(env_names(env), chr())

  env <- env(foo = "foo", bar = "bar")
  env_bind(env, !!!rep_named(c("foo", "bar"), list(zap())))
  expect_identical(env_names(env), chr())

  env <- env(foo = "foo", bar = "bar")
  env_bind_active(env, foo = zap())
  expect_identical(env_names(env), "bar")

  env_bind_lazy(env, bar = !!zap())
  expect_identical(env_names(env), chr())

  env_bind(current_env(), !!!rep_named(c("foo", "bar"), list(zap())))
})

test_that("env_bind_lazy() supports quosures", {
  env <- env()
  foo <- "foo"

  quo <- local({
    foo <- "quux"
    quo(paste(foo, "bar"))
  })

  env_bind_lazy(env, x = !!quo)
  expect_identical(env$x, "quux bar")

  foo <- "FOO"
  expect_identical(env$x, "quux bar")
})

test_that("env_bind_active() supports quosures", {
  env <- env()
  foo <- "foo"

  env_bind_active(env, x = quo(paste(foo, "bar")))
  expect_identical(env$x, "foo bar")

  foo <- "FOO"
  expect_identical(env$x, "FOO bar")
})

test_that("env_bind_lazy() supports nested quosures", {
  env <- env()

  quo <- local({
    lhs <- "quux"
    rhs <- local({
      rhs <- "hunoz"
      quo(rhs)
    })
    quo(paste(lhs, !!rhs))
  })

  env_bind_lazy(env, x = !!quo)
  expect_identical(env$x, "quux hunoz")
})

test_that("env_bind_active() supports nested quosures", {
  env <- env()

  quo <- local({
    lhs <- "quux"
    rhs <- local({
      rhs <- "hunoz"
      quo(rhs)
    })
    quo(paste(lhs, !!rhs))
  })

  env_bind_active(env, x = quo)
  expect_identical(env$x, "quux hunoz")

  quo <- quo_set_env(quo, env(lhs = "QUUX"))
  env_bind_active(env, x = quo)
  expect_identical(env$x, "QUUX hunoz")
})

test_that("env_get() survives native encoding", {
  with_non_utf8_locale({
    e <- env(empty_env())
    funs <- list(function() 42)
    native <- enc2native("\u4e2d")
    s <- as_string(native)

    names(funs) <- native
    env_bind_active(e, !!!funs)
    names(funs) <- s
    env_bind_active(e, !!!funs)
    expect_equal(e[[s]], 42)
    expect_equal(e[[native]], 42)
  })
})

test_that("`env_binding_are_lazy()` type-checks `env` (#923)", {
  expect_error(env_binding_are_lazy("a", "b"), "must be an environment")
})

test_that("env_poke() zaps (#1012)", {
  env <- env(zapzap = 1)
  env_poke(env, "zapzap", zap())
  expect_false(env_has(env, "zapzap"))

  env <- env(env(zapzap = 1))
  env_poke(env, "zapzap", zap())
  expect_false(env_has(env, "zapzap"))
  expect_true(env_has(env, "zapzap", inherit = TRUE))

  env_poke(env, "zapzap", zap(), inherit = TRUE)
  expect_false(env_has(env, "zapzap", inherit = TRUE))
})

test_that("env_poke() doesn't warn when unrepresentable characters are serialised", {
  if (is_windows && getRversion() < "4.2") {
    return()
  }

  with_non_utf8_locale({
    e <- env(empty_env())
    nm <- get_alien_lang_string()

    expect_no_warning(env_poke(e, nm, NA))

    skip_if_no_utf8_marker()
    nms <- env_names(e)
    expect_equal(Encoding(nms), "UTF-8")
  })
})

test_that("new_environment() supports non-list data", {
  env <- new_environment(c(a = 1))
  expect_equal(typeof(env), "environment")
  expect_equal(env$a, 1)
})

test_that("`%<~%` assigns lazily", {
  x %<~% 1
  expect_equal(x, 1)

  x %<~% stop("foo")
  expect_error(x, "foo")

  # Can reassign over a throwing promise
  x %<~% stop("bar")
  expect_error(x, "bar")
})

test_that("env_get() and env_get_list() handle `last` argument", {
  top <- env(foo = "foo")
  mid <- env(top)
  low <- env(mid)

  expect_equal(
    env_get(low, "foo", inherit = TRUE, default = "null"),
    "foo"
  )
  expect_equal(
    env_get(low, "foo", inherit = TRUE, last = mid, default = "null"),
    "null"
  )
  expect_equal(
    env_get(low, "foo", inherit = TRUE, last = low, default = "null"),
    "null"
  )

  expect_equal(
    env_get_list(low, "foo", inherit = TRUE, default = "null"),
    list(foo = "foo")
  )
  expect_equal(
    env_get_list(low, "foo", inherit = TRUE, last = mid, default = "null"),
    list(foo = "null")
  )
  expect_equal(
    env_get_list(low, "foo", inherit = TRUE, last = low, default = "null"),
    list(foo = "null")
  )
})

test_that("env_cache() works (#1081)", {
  e <- env(a = "foo")

  # Returns existing binding
  expect_equal(
    env_cache(e, "a", "default"),
    "foo"
  )

  # Creates a `b` binding and returns its default value
  expect_equal(
    env_cache(e, "b", "default"),
    "default"
  )

  # Now `b` is defined
  expect_equal(e$b, "default")
  expect_equal(e$a, "foo")
})

test_that("env_get(last = ) checks for empty env when last is disconnected (#1208)", {
  out <- env_get(
    emptyenv(),
    "_foobar",
    default = "_fallback",
    inherit = TRUE,
    last = globalenv()
  )
  expect_equal(out, "_fallback")
})
