context("context stack") # -------------------------------------------

test_that("ctxt_stack_callers() agrees with sys.parents()", {
  parents <- sys.parents()
  callers <- ctxt_stack_callers()
  expect_equal(callers, rev(parents))
})
test_that("ctxt_stack_exprs() agrees with sys.call()", {
  pos <- ctxt_pos()
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

# Beware some sys.x() take `n` and some take `which`
test_that("ctxt_caller() agrees with sys.parent()", {
  parent <- sys.parent(n = 1)
  caller <- ctxt_caller()
  expect_equal(caller, parent)
})

test_that("ctxt_expr() agrees with sys.call()", {
  n <- sys.nframe()
  syscall <- sys.call(which = n)
  expr <- ctxt_expr()
  expect_identical(expr, syscall)

  expr <- identity(ctxt_expr())
  expect_equal(expr, quote(identity(ctxt_expr())))
})

test_that("ctxt_env() agrees with sys.frame()", {
  n <- sys.nframe()
  sysframe <- sys.frame(which = n)
  env <- ctxt_env()
  expect_identical(env, sysframe)
})

test_that("context position is correct", {
  pos1 <- identity(ctxt_pos())
  pos2 <- identity(identity(ctxt_pos()))

  pos1 <- fixup_ctxt_pos(pos1)
  expect_equal(pos1, 1)

  pos2 <- fixup_ctxt_pos(pos2)
  expect_equal(pos2, 2)
})

test_that("ctxt_stack_funs() returns functions in correct order", {
  f1 <- function(x) f2(x)
  f2 <- function(x) ctxt_stack_funs()

  funs <- f1()
  funs <- fixup_ctxts(funs)

  expect_identical(funs, list(f1, f2))
})


context("call stack") # ----------------------------------------------

test_that("call_pos() returns correct depth", {
  pos1 <- identity(call_pos())
  expect_equal(fixup_call_pos(pos1), 0)

  f <- function() identity(call_pos())
  g <- function() f()
  pos2 <- f()
  pos3 <- g()
  expect_equal(fixup_call_pos(pos2), 1)
  expect_equal(fixup_call_pos(pos3), 2)

  expect_equal(fixup_call_pos(f()), 1)
  expect_equal(fixup_call_pos(g()), 2)
})

test_that("call_stack_trail() ignores irrelevant frames", {
  f1 <- function(x) f2(x)
  f2 <- function(x) f3()
  f3 <- function(x) call_stack_trail()

  trail1 <- f1()
  expect_equal(fixup_call_trail(trail1), c(3, 2, 1))

  trail2 <- identity(identity(f1()))
  expect_equal(fixup_call_trail(trail2), c(5, 4, 3))
})

test_that("call_env() is the same as parent.frame()", {
  expect_identical(call_env(), parent.frame())
})

test_that("call_stack_envs() gives same result as iterative parent.frame()", {
  iterative_pf <- function() {
    pos <- call_pos() - 1
    out <- vector("list", pos)
    for (i in seq_len(pos)) {
      out[[i]] <- parent.frame(i)
    }
    out
  }

  envs1 <- call_stack_envs()
  envs2 <- iterative_pf()

  expect_identical(envs1, envs2)
})

test_that("call_expr() gives expression of caller not previous ctxt", {
  expr <- call_expr(1)
  expect_equal(expr, sys.call(0))
  expect_equal(identity(call_expr(1)), sys.call(0))

  f <- function(x = 1) call_expr(x)
  expect_equal(f(), quote(f()))

  g <- function() identity(f(2))
  expect_equal(g(), quote(g()))
})

test_that("call_stack_exprs() is in opposite order to sys calls", {
  syscalls <- sys.calls()
  exprs <- call_stack_exprs()
  expect_equal(exprs[[length(exprs)]], syscalls[[1]])
  expect_equal(exprs[[1]], syscalls[[length(syscalls)]])
})


context("stack summaries") # -----------------------------------------

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
