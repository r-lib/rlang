test_that("can return from frame", {
  fn <- function() {
    val <- g()
    paste(val, "to fn()")
  }
  g <- function(env) {
    h(environment())
    stop("g!\n")
  }
  h <- function(env) {
    return_from(env, "returned from h()")
    stop("h!\n")
  }

  expect_equal(fn(), "returned from h() to fn()")
})

test_that("current_env() and current_fn() return current frame props", {
  fn <- function() {
    list(
      rlang = list(identity(current_env()), current_fn()),
      base = list(environment(), sys.function())
    )
  }
  out <- fn()
  expect_identical(out$rlang[[1]], out$base[[1]])
  expect_identical(out$rlang[[2]], out$base[[2]])
})

test_that("sys_parents() removes infloop values", {
  f <- function() g()
  g <- function() sys_parents()

  parents <- do.call("f", list(), envir = env())
  n <- length(parents)

  # No loop when called in non-frame env
  expect_false(any(parents == seq_len(n)))

  # g() is called by f() which is called by global because the calling
  # env is not on the stack
  expect_equal(parents[[n - 1]], 0) # f()
  expect_equal(parents[[n]], n - 1) # g()
})

test_that("current_fn() and caller_fn() work", {
  f <- function(n) identity(g(n))
  g <- function(n) identity(h(n))
  h <- function(n) identity(caller_fn(n))
  expect_equal(f(1), g)
  expect_equal(f(2), f)

  # Need to break the chain of callers to get `NULL` at `n = 3`.
  # Otherwise we get the `eval()` frame from testthat
  expect_null(eval_bare(quote(f(3)), env()))

  f <- function() current_fn()
  expect_equal(f(), f)
})

test_that("Parents are matched to youngest duplicate frames", {
  skip_on_cran()

  out <- env()
  f <- function() {
    invisible(g(environment(), report("f")))
  }
  g <- function(env, arg) {
    fn <- function() h(env, arg)
    eval(as.call(list(fn)), env)
  }
  h <- function(env, arg) {
    fn <- function() list(arg, report("h"))
    eval(as.call(list(fn)), env)
  }
  report <- function(what) {
    parents <- sys_parents(match_oldest = FALSE)
    env_poke(out, what, parents)
  }

  f()
  f_parents <- tail(out[["f"]], 10) - length(out[["f"]]) + 10
  h_parents <- tail(out[["h"]], 10) - length(out[["h"]]) + 10

  expect_equal(f_parents, c(0:8, 1L))
  expect_equal(h_parents, 0:9)
})

test_that("frame_fn() returns the function of the supplied frame", {
  f <- function() {
    identity(g(current_env()))
  }
  g <- function(frame) {
    identity(h(frame))
  }
  h <- function(frame) {
    tryCatch(frame_fn(frame))
  }
  expect_equal(f(), f)

  f <- function() {
    evalq(g(current_env()))
  }
  expect_equal(f(), f)

  f <- function() {
    evalq(g(current_env()), env())
  }
  eval_prim <- eval(call2(sys.function))
  expect_equal(f(), eval_prim)

  f <- function() {
    eval_bare(quote(g(current_env())), env())
  }
  expect_null(f())
})

test_that("current_call(), caller_call() and frame_call() work", {
  expect_null(eval_bare(call2(current_call), global_env()))
  expect_null(eval_bare(call2(caller_call), global_env()))
  expect_null(eval_bare(call2(frame_call), global_env()))

  f <- function() g()
  g <- function() {
    direct <- frame_call()
    indirect <- evalq(frame_call())
    expect_equal(direct, indirect)
  }
  f()

  f <- function() {
    this <- current_call()
    that <- g()
    expect_equal(this, quote(f()))
    expect_equal(this, that)
  }
  g <- function() caller_call()
  f()

  return("Don't make this guarantee to stay consistent with `caller_env()`")
  f <- function() g()
  g <- function() {
    direct <- caller_call()
    indirect <- h(current_env())
    expect_equal(indirect, direct)
  }
  h <- function(env) evalq(caller_call(), env)
  f()
})

test_that("caller_env2() respects invariant", {
  f <- function() h()
  h <- function() {
    indirect <- evalq(caller_env2())
    direct <- caller_env2()
    expect_equal(indirect, direct)
  }
  f()

  f <- function() g()
  g <- function() inject(caller_env2(), env())
  expect_equal(f(), global_env())
})
