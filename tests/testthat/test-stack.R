context("evaluation frames") # ---------------------------------------

# Beware some sys.x() take `n` and some take `which`
test_that("eval_frame() caller agrees with sys.parent()", {
  parent <- sys.parent(n = 1)
  caller <- eval_frame()$caller
  expect_equal(caller, parent)
})

test_that("eval_frame() expr agrees with sys.call()", {
  n <- sys.nframe()
  syscall <- sys.call(which = n)
  expr <- eval_frame()$expr
  expect_identical(expr, syscall)

  frame <- identity(eval_frame())
  expect_equal(frame$expr, quote(identity(eval_frame())))
})

test_that("eval_frame() env agrees with sys.frame()", {
  n <- sys.nframe()
  sysframe <- sys.frame(which = n)
  env <- eval_frame()$env
  expect_identical(env, sysframe)
})

test_that("context position is correct", {
  pos1 <- identity(eval_frame()$pos)
  pos2 <- identity(identity(eval_frame()$pos))

  pos1 <- fixup_eval_depth(pos1)
  expect_equal(pos1, 1)

  pos2 <- fixup_eval_depth(pos2)
  expect_equal(pos2, 2)
})

test_that("eval_frame(n_depth) returns global frame", {
  n_depth <- eval_depth()
  frame <- eval_frame(n_depth)
  global <- global_frame()
  expect_identical(frame, global)
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

test_that("call_frame()$env is the same as parent.frame()", {
  f <- function(n) call_frame(n + 1)$env
  f_base <- function(n) parent.frame(n)
  env1 <- f(1)
  env1_base <- f_base(1)
  expect_identical(env1, env1_base)

  g <- function(n) list(f(n), f_base(n))
  envs <- g(1)
  expect_identical(envs[[1]], envs[[2]])
})

test_that("call_frame()$expr gives expression of caller not previous ctxt", {
  f <- function(x = 1) call_frame(x)$expr
  expect_equal(f(), quote(f()))

  g <- function() identity(f(2))
  expect_equal(g(), quote(g()))
})

test_that("call_frame(n_depth) returns global frame", {
  n_depth <- call_depth()
  expect_identical(call_frame(n_depth), global_frame())
})

test_that("call_frame(n) throws at correct level", {
  n <- call_depth()
  expect_error(call_frame(n + 1), "not that many frames")
})

test_that("call frames are cleaned", {
  eval_frame_messy <- eval(quote(call_frame(clean = FALSE)), new.env())
  expect_identical(eval_frame_messy$fn, prim_eval)

  eval_frame_clean <- eval(quote(call_frame(clean = TRUE)), new.env())
  expect_identical(eval_frame_clean$fn, base::eval)
})


context("evaluation stacks") # ---------------------------------------

test_that("eval_stack_callers() agrees with sys.parents()", {
  parents <- sys.parents()
  callers <- eval_stack_callers()
  expect_equal(callers, rev(parents))
})

test_that("eval_stack_exprs() agrees with sys.call()", {
  pos <- sys.nframe()
  syscalls <- lapply(seq(pos, 1), sys.call)
  exprs <- eval_stack_exprs()
  expect_identical(exprs, syscalls)
})

test_that("eval_stack_envs() agrees with sys.frames()", {
  sysframes <- sys.frames()
  sysframes <- rev(as.list(sysframes))
  envs <- eval_stack_envs()
  expect_identical(envs, sysframes)
})

test_that("eval_stack_trail() returns a vector of size nframe", {
  trail <- eval_stack_trail()
  n <- sys.nframe()
  expect_equal(length(trail), n)
})

test_that("eval_stack_fns() returns functions in correct order", {
  f1 <- function(x) f2(x)
  f2 <- function(x) eval_stack_fns()
  expect_identical(f1()[1:2], list(f2, f1))
})

test_that("eval_stack_fns() handles intervening frames", {
  fns <- eval_stack_fns()
  intervened_fns <- identity(identity(eval_stack_fns()))
  expect_identical(c(identity, identity, fns), intervened_fns)
})

test_that("eval_stack() handles intervening frames", {
  stack <- eval_stack()
  intervened_stack <- identity(eval_stack())[-1]
  expect_identical(intervened_stack, stack)
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

test_that("eval_stack() exprs is in opposite order to sys calls", {
  syscalls <- sys.calls()
  stack <- eval_stack()
  stack <- drop_last(stack) # global frame
  exprs <- purrr::map(stack, "expr")
  expect_equal(exprs[[length(exprs)]], syscalls[[1]])
  expect_equal(exprs[[1]], syscalls[[length(syscalls)]])
})

test_that("eval_stack() and call_stack() agree", {
  call_stack <- call_stack()
  call_stack <- drop_last(call_stack) # global frame
  positions <- vapply_int(call_stack, `[[`, "pos")

  eval_stack <- eval_stack()
  eval_stack <- drop_last(eval_stack) # global frame
  eval_stack <- rev(eval_stack)[positions]

  call_exprs <- lapply(call_stack, `[[`, "expr")
  eval_exprs <- lapply(eval_stack, `[[`, "expr")
  expect_identical(call_exprs, eval_exprs)

  is_eval <- vapply_lgl(call_stack, function(frame) {
    identical(frame$fn, base::eval)
  })

  call_envs <- lapply(call_stack[!is_eval], `[[`, "env")
  eval_envs <- lapply(eval_stack[!is_eval], `[[`, "env")
  expect_identical(call_envs, eval_envs)
})

test_that("eval_stack() subsets n frames", {
  stack <- eval_stack()
  stack_2 <- eval_stack(2)
  expect_identical(stack_2, stack[1:2])

  n <- eval_depth()
  stack_n <- eval_stack(n)
  expect_identical(stack_n, stack)

  # Get correct eval depth within expect_error()
  expect_error({ n <- eval_depth(); stop() })
  expect_error(eval_stack(n + 1), "not that many frames")
})

test_that("call_stack() subsets n frames", {
  stack <- call_stack()
  stack_2 <- call_stack(2)
  expect_identical(stack_2, stack[1:2])

  n <- call_depth()
  stack_n <- call_stack(n)
  expect_identical(stack_n, stack)

  # Get correct eval depth within expect_error()
  expect_error({ n <- call_depth(); stop() })
  expect_error(call_stack(n + 1), "not that many frames")
})

test_that("call stacks are cleaned", {
  stack_messy <- eval(quote(call_stack(clean = FALSE)), new.env())[1:2]
  expect_identical(stack_messy[[1]]$fn, prim_eval)
  expect_identical(stack_messy[[2]]$fn, base::eval)

  stack_clean <- eval(quote(call_stack(clean = TRUE)), new.env())
  expect_identical(stack_clean[[1]]$fn, base::eval)
})


context("frame utils") # ---------------------------------------------

test_that("frame_position() returns correct position", {
  fn <- function() {
    env <- environment()
    pos <- eval_frame()$pos
    g(env, pos)
  }
  g <- function(env, fn_pos) {
    pos <- frame_position(env)
    expect_identical(pos, fn_pos)

    burried_pos <- identity(identity(frame_position(env)))
    expect_identical(burried_pos, pos)
  }
  fn()
})

test_that("frame_position_current() computes distance from a frame", {
  fn <- function() {
    g(environment())
  }
  g <- function(env) {
    distance <- frame_position(env, from = "current")
    frame <- eval_frame(distance)
    expect_identical(frame$env, env)

    burried_distance <- identity(frame_position(env, from = "current"))
    expect_equal(distance, burried_distance)
  }
  fn()
})

test_that("evaluation stack is trimmed from layers of calls", {
  stack <- eval_stack()
  trimmed_stack <- identity(stack_trim(identity(eval_stack())))
  expect_identical(stack, trimmed_stack)
})

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

test_that("can return to frame", {
  fn <- function() {
    val <- identity(g(environment()))
    paste(val, "to fn()")
  }
  g <- function(env) {
    h(env)
    stop("g!\n")
  }
  h <- function(env) {
    return_to(env, "returned from h()")
    stop("h!\n")
  }

  expect_equal(fn(), "returned from h() to fn()")
})
