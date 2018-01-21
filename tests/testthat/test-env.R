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

test_that("env_bind_fns() creates active bindings", {
  env <- env_bind_fns(env(), a = function() "foo")
  expect_identical(env$a, "foo")
  expect_identical(env$a, "foo")
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

test_that("as_environment() dispatches correctly", {
  expect_identical(as_environment("base"), base_env())
  expect_false(env_has(as_environment(set_names(letters)), "map"))

  expect_identical(as_environment(NULL), empty_env())

  expect_true(all(env_has(as_environment(mtcars), names(mtcars))))
  expect_identical(env_parent(as_environment(mtcars)), empty_env())
  expect_identical(env_parent(as_environment(mtcars, base_env())), base_env())
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

test_that("env_poke() returns env", {
  env <- child_env(new_environment())
  expect_identical(env_poke(env, "foo", "foo"), env)
  expect_identical(env_poke(env, "foo", "foo", inherit = TRUE), env)
})

test_that("env_poke() creates binding if `create` is TRUE", {
  env <- new_environment()
  expect_identical(env_get(env_poke(env, "foo", "foo"), "foo"), "foo")

  expect_error(env_poke(env, "bar", "BAR", create = FALSE), "Can't find existing binding")
  expect_identical(env_get(env_poke(env, "foo", "FOO", create = FALSE), "foo"), "FOO")
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

test_that("env_tail() detects sentinel", {
  sentinel <- get_env()
  env <- env()
  descendant <- child_env(child_env(child_env(env)))
  expect_identical(env_tail(descendant, sentinel), env)
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

test_that("env_unbind() with `inherits = TRUE` wipes out all bindings", {
  bindings <- list(`_foo` = "foo", `_bar` = "bar")
  env_bind(global_env(), !!! bindings)
  env <- child_env(global_env(), !!! bindings)

  env_unbind(env, "_foo", inherit = TRUE)
  expect_false(all(env_has(env, names(bindings))))
  expect_false(all(env_has(global_env(), names(bindings))))
})

test_that("env_names() unserialises unicode", {
  env <- env(`<U+5E78><U+798F>` = "foo")
  expect_identical(env_names(env), "\u5E78\u798F")
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

test_that("is_namespace() recognises namespaces", {
  expect_false(is_namespace(env()))
  expect_true(is_namespace(get_env(is_namespace)))
})
