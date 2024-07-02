test_that("env_parent() returns enclosure frame by default", {
  enclos_env <- child_env(pkg_env("rlang"))
  fn <- with_env(enclos_env, function() env_parent())
  expect_identical(fn(), enclos_env)
})

test_that("child_env() has correct parent", {
  env <- child_env(empty_env())
  expect_false(env_has(env, "list", inherit = TRUE))

  fn <- function() list(new = child_env(current_env()), env = environment())
  out <- fn()
  expect_identical(env_parent(out$new), out$env)

  expect_identical(env_parent(child_env(NULL)), empty_env())
  expect_identical(env_parent(child_env("base")), base_env())
})

test_that("env_parent() reports correct parent", {
  env <- child_env(child_env(NULL, obj = "b"), obj = "a")

  expect_identical(env_parent(env, 1)$obj, "b")
  expect_identical(env_parent(env, 2), empty_env())
  expect_error(env_parent(env, 3), "empty environment has no parent")
})

test_that("env_tail() climbs env chain", {
  expect_identical(env_tail(global_env()), base_env())
})

test_that("env_tail() stops at the global env", {
  tail <- env(global_env())
  env <- env(tail)
  expect_identical(env_tail(env), tail)
})

test_that("with_env() evaluates within correct environment", {
  fn <- function() {
    g(current_env())
    "normal return"
  }
  g <- function(env) {
    with_env(env, return("early return"))
  }
  expect_equal(fn(), "early return")
})

test_that("locally() evaluates within correct environment", {
  env <- child_env("rlang")
  local_env <- with_env(env, locally(current_env()))
  expect_identical(env_parent(local_env), env)
})

test_that("as_environment() dispatches correctly", {
  expect_identical(as_environment("base"), base_env())
  expect_false(env_has(as_environment(set_names(letters)), "map"))

  expect_identical(as_environment(NULL), empty_env())

  expect_true(all(env_has(as_environment(mtcars), names(mtcars))))
  expect_identical(env_parent(as_environment(mtcars)), empty_env())
  expect_identical(env_parent(as_environment(mtcars, base_env())), base_env())
})

test_that("env_inherits() finds ancestor", {
  env <- env(env(current_env()))
  expect_true(env_inherits(env, current_env()))
  expect_false(env_inherits(env, ns_env("utils")))
})

test_that("env_inherits() detects empty environment", {
  expect_false(env_inherits(empty_env(), empty_env()))
  expect_true(env_inherits(env(empty_env()), empty_env()))
})

test_that("env() creates child of current environment", {
  env <- env(a = 1, b = "foo")
  expect_identical(env_parent(env), current_env())
  expect_identical(env$b, "foo")
})

test_that("set_env() sets current env by default", {
  quo <- set_env(locally(~foo))
  expect_identical(f_env(quo), current_env())
})

test_that("finds correct env type", {
  expect_identical(env_type(global_env()), "global")
  expect_identical(env_type(empty_env()), "empty")
  expect_identical(env_type(base_env()), "base")
})

test_that("current_env() fails if no default", {
  expect_error(get_env(list()), "Can't extract an environment from")
})

test_that("current_env() picks up default", {
  dft <- env()
  expect_identical(get_env(list(), dft), dft)
  expect_identical(get_env("a", dft), dft)
})

test_that("with_env() handles data", {
  expect_identical(with_env(mtcars, cyl), mtcars$cyl)

  foo <- "foo"
  expect_identical(with_env(mtcars, foo), "foo")
})

test_that("with_env() evaluates in env", {
  env <- env()
  expect_identical(with_env(env, current_env()), env)
})

test_that("env_depth() counts parents", {
  expect_identical(env_depth(child_env(child_env(NULL))), 2L)
  expect_identical(env_depth(empty_env()), 0L)
})

test_that("env_parents() returns all parents", {
  expect_identical(env_parents(empty_env()), new_environments(list()))
  env1 <- env(empty_env())
  env2 <- env(env1)
  expect_identical(env_parents(env2), new_environments(list(env1, empty_env())))
})

test_that("env() doesn't partial match on env_bind()'s .env", {
  expect_true(all(env_has(env(.data = 1, . = 2), c(".data", "."))))
})

test_that("new_environment() creates a child of the empty env", {
  env <- new_environment(list(a = 1, b = 2))
  expect_true(all(env_has(env, c("a", "b"))))
  expect_identical(env_parent(env), empty_env())
})

test_that("new_environment() accepts empty vectors", {
  expect_identical(length(new_environment()), 0L)
  expect_identical(length(new_environment(dbl())), 0L)
})

test_that("env_tail() detects sentinel", {
  sentinel <- current_env()
  env <- env()
  descendant <- child_env(child_env(child_env(env)))
  expect_identical(env_tail(descendant, sentinel), env)
})

test_that("as_environment() treats named strings as vectors", {
  env <- as_environment(c(foo = "bar"))
  expect_true(is_environment(env))
  expect_true(env_has(env, "foo"))
})

test_that("as_environment() converts character vectors", {
  env <- as_environment(set_names(letters))
  expect_true(is_environment(env))
  expect_true(all(env_has(env, letters)))
})

test_that("child_env() requires named elements", {
  expect_error(child_env(env(), 1), "some elements are not named")
})

test_that("env() requires named elements", {
  expect_error(env(env(), 1), "Expected 0 or 1 unnamed arguments")
})

test_that("env() doesn't require uniquely named elements", {
  env <- env(a = 1, a = 2)
  expect_identical(env$a, 2)
})

test_that("env_clone() clones an environment", {
  data <- list(a = 1L, b = 2L)
  env <- env(!!! data)
  clone <- env_clone(env)
  expect_false(is_reference(env, clone))
  expect_reference(env_parent(env), env_parent(clone))
  expect_identical(env_get_list(clone, c("a", "b")), data)
})

test_that("friendly_env_type() returns a friendly env name", {
  expect_identical(friendly_env_type("global"), "the global environment")
  expect_identical(friendly_env_type("empty"), "the empty environment")
  expect_identical(friendly_env_type("base"), "the base environment")
  expect_identical(friendly_env_type("frame"), "a frame environment")
  expect_identical(friendly_env_type("local"), "a local environment")
})

test_that("new_environment() accepts optional parent", {
  env <- new_environment(parent = base_env())
  expect_reference(env_parent(env), base_env())
})

test_that("env() accepts one unnamed argument to specify parent", {
  env <- env(base_env())
  expect_reference(env_parent(env), base_env())

  env <- env(global_env(), a = 1)
  expect_reference(env_parent(env), global_env())
  expect_identical(env_names(env), "a")
})

test_that("env_parents() stops at the global env by default", {
  env <- env(env(global_env()))
  expect_identical(env_parents(env), new_environments(list(env_parent(env), global_env())))

  rlang_parents <- env_parents(ns_env("rlang"))
  expected <- list(`namespace:base` = ns_env("base"), global = global_env())
  expect_identical(unclass(rlang_parents[2:3]), expected)
})

test_that("env_parents() always stops at the empty env", {
  expect_identical(env_parents(empty_env()), new_environments(list()))
  expect_identical(env_parents(pkg_env("base")), new_environments(list(empty_env())))
})

test_that("env_parents() stops at the sentinel if supplied", {
  expect_reference(last(env_parents(pkg_env("utils"))), empty_env())
  expect_reference(last(env_parents(pkg_env("utils"), base_env())), base_env())
})

test_that("env_parents() returns a named list", {
  env <- env(structure(env(base_env()), name = "foobar"))
  expect_identical(names(env_parents(env)), c("foobar", "package:base", "empty"))
})

test_that("can lock environments", {
  env <- env()
  expect_false(env_is_locked(env))

  expect_false(env_lock(env))
  expect_true(env_is_locked(env))

  expect_true(env_lock(env))
})

test_that("env_print() has flexible input", {
  # because it's primarily used interactively
  f <- function() 1
  expect_output(env_print(f), "environment: ")
})

test_that("active and promise bindings are pretty-printed", {
  env <- env()
  env_bind_lazy(env, a = "foo")
  env_bind_active(env, b = ~"foo")
  expect_output(env_print(env), "a: <lazy>.*b: <active>")
})

test_that("locked environments are pretty-printed", {
  env <- env()
  expect_output(env_print(env), sprintf("<environment: %s>\n", obj_address(env)))
  env_lock(env)
  expect_output(env_print(env), sprintf("<environment: %s> \\[L\\]\n", obj_address(env)))
})

test_that("locked bindings are pretty-printed", {
  env <- env(a = 1, b = 2)
  env_binding_lock(env, "a")
  expect_output(env_print(env), "a: <dbl> \\[L\\].*b: <dbl>")
})

test_that("large environments are truncated", {
  n_truncated <- length(env_names(base_env())) - 20L
  expected <- sprintf("\\.\\.\\. with %s more bindings", n_truncated)
  expect_output(env_print(base_env()), expected)
})

test_that("special names are backticked", {
  env <- env(`<-` = 1, `:` = 2)
  expect_output(env_print(env), "`:`:")
  expect_output(env_print(env), "`<-`:")
})

test_that("empty environment is pretty printed", {
  expect_output(env_print(empty_env()), "<environment: empty>\nParent: NULL$")
})

test_that("envs printer: padding is added to right-align indices", {
  x <- c(rep(list(empty_env()), 9L), global_env())
  x <- new_environments(x)
  expect_output(print(x), "^ \\[\\[1\\]\\]")
  expect_output(print(x), "\n\\[\\[10\\]\\]")
})

test_that("envs printer: name tag is added to named elements", {
  x <- list(empty_env(), env(), empty_env())
  x <- new_environments(x)
  expect_output(print(x), "[[1]] $ <", fixed = TRUE)
  expect_output(print(x), "\n[[2]]   <", fixed = TRUE)
  expect_output(print(x), "\n[[3]] $ <", fixed = TRUE)
})

test_that("envs printer: no name tag if no named elements", {
  x <- list(env(), env())
  x <- new_environments(x)
  expect_output(print(x), "[[1]] <", fixed = TRUE)
  expect_output(print(x), "\n[[2]] <", fixed = TRUE)

  names(x) <- c("", NA)
  expect_output(print(x), "[[1]] <", fixed = TRUE)
  expect_output(print(x), "\n[[2]] <", fixed = TRUE)
})

test_that("envs printer: long lists are truncated", {
  x <- rep(list(empty_env()), 20L)
  x <- new_environments(x)
  expect_output(print(x), "empty>$")

  x <- rep(list(empty_env()), 25L)
  x <- new_environments(x)
  expect_output(print(x), "empty>\n... and 5 more environments$")
})

test_that("can print environment containing missing argument", {
  env <- env(x = missing_arg(), y = quote(foo))
  expect_output(env_print(env), "x: <missing>")
  expect_output(env_print(env), "y: <sym>")
})

test_that("parent environment is printed with full header", {
  env <- env(global_env())
  expect_output(env_print(env), "Parent: <environment: global>")
})

test_that("environment is printed with class if any", {
  env <- env()
  out <- capture.output(env_print(env))
  expect_false(any(grepl("class", out)))

  env <- structure(env(), class = "foo")
  expect_output(env_print(env), "Class: foo")

  env <- structure(env(), class = c("foo", "bar"))
  expect_output(env_print(env), "Class: foo, bar")
})

test_that("env_clone() handles active bindings", {
  # FIXME: Seems cloning evaluates the binding
  value <- NULL

  e <- env()
  env_bind_active(e, foo = function() value)
  out <- env_clone(e)

  value <- "foo"
  expect_equal(out$foo, "foo")

  value <- "bar"
  expect_equal(out$foo, "bar")
})

test_that("env_clone() doesn't force promises", {
  skip_if_not_installed("base", "4.0.0")

  e <- env()
  env_bind_lazy(e, foo = value)

  value <- "foo"
  out <- env_clone(e)

  value <- "bar"
  expect_equal(out$foo, "bar")
})

test_that("env_poke_parent() pokes parent", {
  e <- env()
  env_poke_parent(e, empty_env())
  expect_reference(env_parent(e), empty_env())
})

test_that("env_poke_parent() fails with namespaces, package envs, and locked envs", {
  expect_error(env_poke_parent(ns_env("rlang"), env()), "namespace environment")
  expect_error(env_poke_parent(pkg_env("rlang"), env()), "package environment")

  expect_error(env_poke_parent(global_env(), env()), "global")
  expect_error(env_poke_parent(empty_env(), env()), "empty")
  expect_error(env_poke_parent(base_env(), env()))

  env <- env()
  env_lock(env)
  expect_error(env_poke_parent(env, env()), "locked environment")
})

test_that("env_length() gives env length", {
  expect_error(env_length(1), "must be an environment")
  expect_identical(env_length(env()), 0L)
  expect_identical(env_length(env(a = "a")), 1L)
})

test_that("env_clone() increases refcounts (#621)", {
  e <- env(x = 1:2)
  env_bind_lazy(e, foo = 1)
  env_bind_active(e, bar = function() 1)

  c <- env_clone(e)
  c$x[1] <- NA

  expect_identical(e$x, c(1L, 2L))
  expect_identical(c$x, c(NA, 2L))
})

test_that("env_coalesce() merges environments", {
  x <- env(x = 1, y = 2)
  y <- env(x = "a", z = "c")

  env_coalesce(x, y)

  expect_equal(x, env(x = 1, y = 2, z = "c"))
  expect_equal(y, env(x = "a", z = "c"))
})

test_that("env_coalesce() handles fancy bindings", {
  old_r <- getRversion() < "4.0.0"

  x <- env(x = 1, y = 2)
  y <- env(x = "a", z = "c")
  env_bind_lazy(y, lazy = { signal("", "lazy"); "lazy-value" })
  env_bind_active(y, active = function() { signal("", "active"); "active-value" })

  env_coalesce(x, y)

  if (!old_r) {
    expect_condition(
      expect_equal(x$lazy, "lazy-value"),
      class = "lazy"
    )
  }
  expect_condition(
    expect_equal(x$active, "active-value"),
    class = "active"
  )

  expect_equal(x$x, 1)
  expect_equal(x$y, 2)
  expect_equal(x$z, "c")
  expect_equal(x$active, "active-value")
  expect_equal(x$lazy, "lazy-value")

  # `y$lazy` was forced at the same time as `x$lazy`
  expect_false(env_binding_are_lazy(y, "lazy"))

  expect_condition(
    expect_equal(y$active, "active-value"),
    class = "active"
  )

  expect_equal(y$x, "a")
  expect_equal(y$z, "c")
  expect_equal(y$active, "active-value")
  expect_equal(y$lazy, "lazy-value")
})

test_that("can subset `rlang_envs` list", {
  envs <- new_environments(list(env(), env(), env()))

  out <- envs[1:2]
  expect_length(out, 2)
  expect_s3_class(out, "rlang_envs")

  out <- envs[3]
  expect_length(out, 1)
  expect_s3_class(out, "rlang_envs")
})

test_that("can concatenate `rlang_envs` lists", {
  envs1 <- new_environments(list(env()))
  envs2 <- new_environments(list(env(), env()))
  out <- c(envs1, envs2)
  expect_length(out, 3)
  expect_s3_class(out, "rlang_envs")
})

test_that("env_name() requires an environment", {
  expect_error(env_name("base"), "must be an environment")
})

test_that("env_unbind() removes objects", {
  env <- env(a = 1L)
  env_unbind(env, "a")
  expect_false(env_has(env, "a"))

  env <- env(a = 1L)
  child <- child_env(env)

  env_unbind(child, "a")
  expect_true(env_has(child, "a", inherit = TRUE))

  env_unbind(child, "a", inherit = TRUE)
  expect_false(env_has(env, "a"))
})

test_that("get_env() returns the base namespace for primitive functions (r-lib/downlit#32)", {
  expect_identical(get_env(is.null), ns_env("base"))
})

test_that("can browse environments", {
  env <- env()
  expect_false(env_is_browsed(env))

  old <- env_browse(env)
  expect_false(old)
  expect_true(env_is_browsed(env))

  old <- env_browse(env, FALSE)
  expect_true(old)
  expect_false(env_is_browsed(env))
})

test_that("env_has() doesn't force active bindings (#1292)", {
  e <- env()
  env_bind_active(e, active = function() abort("forced"))

  expect_true(env_has(e, "active"))

  e2 <- env(e)
  expect_true(env_has(e2, "active", inherit = TRUE))
})

test_that("env_is_user_facing() detects direct usage from the global env", {
  expect_true(env_is_user_facing(global_env()))
  expect_true(env_is_user_facing(env(global_env())))

  expect_false(env_is_user_facing(ns_env("base")))
  expect_false(env_is_user_facing(ns_env("testthat")))
})

test_that("env_is_user_facing() detects direct usage in tests", {
  # Simulate `devtools::test()` and `devtools::check()` envs to allow
  # direct interactive evaluation
  env <- env(ns_env("rlang"))

  expect_true(from_testthat(env))
  expect_true(env_is_user_facing(env))

  withr::with_envvar(
    c(TESTTHAT_PKG = "foo"),
    {
      expect_false(from_testthat(env))
      expect_false(env_is_user_facing(env))
    }
  )
})

test_that("env_is_user_facing() can be overridden", {
  with_options(
    rlang_user_facing = TRUE,
    expect_true(env_is_user_facing(empty_env()))
  )

  with_options(
    rlang_user_facing = FALSE,
    expect_false(env_is_user_facing(empty_env()))
  )

  with_options(
    rlang_user_facing = "utils",
    expect_true(env_is_user_facing(ns_env("utils")))
  )

  expect_snapshot({
    options(rlang_user_facing = NA)
    (expect_error(env_is_user_facing(empty_env())))
    expect_null(peek_option("rlang_user_facing"))
  })
})
