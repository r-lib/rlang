context("evaluation frames") # ---------------------------------------

# Beware some sys.x() take `n` and some take `which`
test_that("ctxt_frame() caller agrees with sys.parent()", {
  parent <- sys.parent(n = 1)
  caller <- ctxt_frame()$caller
  expect_equal(caller, parent)
})

test_that("ctxt_frame() expr agrees with sys.call()", {
  n <- sys.nframe()
  syscall <- sys.call(which = n)
  expr <- ctxt_frame()$expr
  expect_identical(expr, syscall)

  frame <- identity(ctxt_frame())
  expect_equal(frame$expr, quote(identity(ctxt_frame())))
})

test_that("ctxt_frame() env agrees with sys.frame()", {
  n <- sys.nframe()
  sysframe <- sys.frame(which = n)
  env <- ctxt_frame()$env
  expect_identical(env, sysframe)
})

test_that("context position is correct", {
  pos1 <- identity(ctxt_frame()$pos)
  pos2 <- identity(identity(ctxt_frame()$pos))

  pos1 <- fixup_ctxt_depth(pos1)
  expect_equal(pos1, 1)

  pos2 <- fixup_ctxt_depth(pos2)
  expect_equal(pos2, 2)
})

test_that("call_depth() returns correct depth", {
  depth1 <- identity(call_depth())
  expect_equal(fixup_call_depth(depth1), 0)

  f <- function() identity(call_depth())
  g <- function() f()
  depth2 <- f()
  depth3 <- g()
  expect_equal(fixup_call_depth(depth2), 1)
  expect_equal(fixup_call_depth(depth3), 2)

  expect_equal(fixup_call_depth(f()), 1)
  expect_equal(fixup_call_depth(g()), 2)
})

test_that("call_env() is the same as parent.frame()", {
  expect_identical(call_frame()$env, parent.frame())
})

test_that("call_expr() gives expression of caller not previous ctxt", {
  expr <- call_frame(1)$expr
  expect_equal(expr, sys.call(0))
  expect_equal(identity(call_frame(1)$expr), sys.call(0))

  f <- function(x = 1) call_frame(x)$expr
  expect_equal(f(), quote(f()))

  g <- function() identity(f(2))
  expect_equal(g(), quote(g()))
})


context("evaluation stacks") # ---------------------------------------

test_that("ctxt_stack_callers() agrees with sys.parents()", {
  parents <- sys.parents()
  callers <- ctxt_stack_callers()
  expect_equal(callers, rev(parents))
})

test_that("ctxt_stack_exprs() agrees with sys.call()", {
  pos <- sys.nframe()
  syscalls <- lapply(seq(pos, 1), sys.call)
  exprs <- ctxt_stack_exprs()
  expect_identical(exprs, syscalls)
})

test_that("ctxt_stack_envs() agrees with sys.frames()", {
  sysframes <- sys.frames()
  sysframes <- rev(as.list(sysframes))
  envs <- ctxt_stack_envs()
  expect_identical(envs, sysframes)
})

test_that("ctxt_stack_trail() returns a vector of size nframe", {
  trail <- ctxt_stack_trail()
  n <- sys.nframe()
  expect_equal(length(trail), n)
})

test_that("ctxt_stack_funs() returns functions in correct order", {
  f1 <- function(x) f2(x)
  f2 <- function(x) ctxt_stack_funs()

  funs <- f1()
  funs <- fixup_ctxts(funs)

  expect_identical(funs, list(f1, f2))
})

test_that("call_stack() trail ignores irrelevant frames", {
  f1 <- function(x) f2(x)
  f2 <- function(x) f3()
  f3 <- function(x) call_stack()

  stack1 <- f1()
  trail1 <- purrr::map_int(stack1, "pos")
  expect_equal(fixup_call_trail(trail1), c(3, 2, 1))

  stack2 <- identity(identity(f1()))
  trail2 <- purrr::map_int(stack2, "pos")
  expect_equal(fixup_call_trail(trail2), c(5, 4, 3))
})

test_that("call_stack() envs give same result as iterative parent.frame()", {
  iterative_pf <- function() {
    pos <- call_depth() - 1
    out <- vector("list", pos)
    for (i in seq_len(pos)) {
      out[[i]] <- parent.frame(i)
    }
    out
  }

  stack <- call_stack()
  envs1 <- purrr::map(stack, "env")
  envs2 <- iterative_pf()

  expect_identical(envs1, envs2)
})

test_that("call_stack() exprs is in opposite order to sys calls", {
  syscalls <- sys.calls()
  exprs <- purrr::map(call_stack(), "expr")
  expect_equal(exprs[[length(exprs)]], syscalls[[1]])
  expect_equal(exprs[[1]], syscalls[[length(syscalls)]])
})

test_that("ctxt_stack() and call_stack() agree", {
  call_stack <- call_stack()
  positions <- vapply_int(call_stack, `[[`, "pos")

  ctxt_stack <- ctxt_stack()
  ctxt_stack <- rev(ctxt_stack)[positions]

  call_exprs <- lapply(call_stack, `[[`, "expr")
  ctxt_exprs <- lapply(ctxt_stack, `[[`, "expr")
  expect_identical(call_exprs, ctxt_exprs)

  call_envs <- lapply(call_stack, `[[`, "env")
  ctxt_envs <- lapply(ctxt_stack, `[[`, "env")
  expect_identical(call_envs, ctxt_envs)
})
