context("environments")

test_that("get_env() returns current frame by default", {
  fn <- function() expect_identical(get_env(), environment())
  fn()
})

test_that("env_parent() returns enclosure frame by default", {
  enclos_env <- child_env(pkg_env("rlang"))
  fn <- with_env(enclos_env, function() env_parent())
  expect_identical(fn(), enclos_env)
})

test_that("child_env() has correct parent", {
  env <- child_env(empty_env())
  expect_false(env_has(env, "list", inherit = TRUE))

  fn <- function() list(new = child_env(get_env()), env = environment())
  out <- fn()
  expect_identical(env_parent(out$new), out$env)

  expect_identical(env_parent(child_env(NULL)), empty_env())
  expect_identical(env_parent(child_env("base")), base_env())
})

test_that("env_parent() reports correct parent", {
  env <- child_env(child_env(NULL, obj = "b"), obj = "a")

  expect_identical(env_parent(env, 1)$obj, "b")
  expect_identical(env_parent(env, 2), empty_env())
  expect_identical(env_parent(env, 3), empty_env())
})

test_that("env_tail() climbs env chain", {
  expect_identical(env_tail(global_env()), base_env())
})

test_that("promises are created", {
  env <- child_env(NULL)

  env_bind_exprs(env, foo = bar <- "bar")
  expect_false(env_has(get_env(), "bar"))

  force(env$foo)
  expect_true(env_has(get_env(), "bar"))

  env_bind_exprs(env, stop = stop("forced"))
  expect_error(env$stop, "forced")
})

test_that("with_env() evaluates within correct environment", {
  fn <- function() {
    g(get_env())
    "normal return"
  }
  g <- function(env) {
    with_env(env, return("early return"))
  }
  expect_equal(fn(), "early return")
})

test_that("locally() evaluates within correct environment", {
  env <- child_env("rlang")
  local_env <- with_env(env, locally(get_env()))
  expect_identical(env_parent(local_env), env)
})

test_that("ns_env() returns current namespace", {
  expect_identical(with_env(ns_env("rlang"), ns_env()), get_env(rlang::get_env))
})

test_that("ns_imports_env() returns imports env", {
  expect_identical(with_env(ns_env("rlang"), ns_imports_env()), env_parent(get_env(rlang::get_env)))
})

test_that("ns_env_name() returns namespace name", {
  expect_identical(with_env(ns_env("base"), ns_env_name()), "base")
  expect_identical(ns_env_name(rlang::get_env), "rlang")
})

test_that("as_env() dispatches correctly", {
  expect_identical(as_env("base"), base_env())
  expect_false(env_has(as_env(set_names(letters)), "map"))

  expect_identical(as_env(NULL), empty_env())

  expect_true(all(env_has(as_env(mtcars), names(mtcars))))
  expect_identical(env_parent(as_env(mtcars)), empty_env())
  expect_identical(env_parent(as_env(mtcars, base_env())), base_env())
})

test_that("env_inherits() finds ancestor", {
  env <- child_env(get_env())
  env <- child_env(env)
  expect_true(env_inherits(env, get_env()))
  expect_false(env_inherits(env, ns_env("utils")))

  expect_true(env_inherits(empty_env(), empty_env()))
})

test_that("env() creates child of current environment", {
  env <- env(a = 1, b = "foo")
  expect_identical(env_parent(env), get_env())
  expect_identical(env$b, "foo")
})

test_that("set_env() sets current env by default", {
  quo <- set_env(locally(~foo))
  expect_identical(f_env(quo), get_env())
})

test_that("finds correct env type", {
  expect_identical(identity(env_type(ctxt_frame(2)$env)), "frame")
  expect_identical(env_type(global_env()), "global")
  expect_identical(env_type(empty_env()), "empty")
  expect_identical(env_type(base_env()), "base")
})

test_that("get_env() fails if no default", {
  expect_error(get_env(list()), "Can't extract an environment from a list")
})

test_that("get_env() picks up default", {
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
  expect_identical(with_env(env, get_env()), env)
})

test_that("env_depth() counts parents", {
  expect_identical(env_depth(child_env(child_env(NULL))), 2L)
  expect_identical(env_depth(empty_env()), 0L)
})

test_that("env_parents() returns all parents", {
  expect_identical(env_parents(empty_env()), ll())
  env1 <- child_env(NULL)
  env2 <- child_env(env1)
  expect_identical(env_parents(env2), ll(env1, empty_env()))
})

test_that("scoped_envs() includes global and empty envs", {
  envs <- scoped_envs()
  expect_identical(envs[[1]], global_env())
  expect_identical(envs[[length(envs)]], empty_env())
})

test_that("scoped_envs() returns named environments", {
  expect_identical(names(scoped_envs()), scoped_names())
})

test_that("scoped_env() deals with empty environment", {
  expect_identical(scoped_env("NULL"), empty_env())
})

test_that("env() doesn't partial match on env_bind()'s .env", {
  expect_named(env(.data = 1, . = 2), c(".data", "."))
})

test_that("new_environment() creates a child of the empty env", {
  env <- new_environment(list(a = 1, b = 2))
  expect_named(env, c("a", "b"))
  expect_identical(env_parent(env), empty_env())
})

test_that("new_environment() accepts empty vectors", {
  expect_named(new_environment(), chr())
  expect_named(new_environment(dbl()), chr())
})
