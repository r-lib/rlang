context("retired")

scoped_lifecycle_silence()


# Deprecated in rlang 0.4.0 ------------------------------------------

test_that("type_of() returns correct type", {
  expect_identical(type_of("foo"), "string")
  expect_identical(type_of(letters), "character")
  expect_identical(type_of(base::`$`), "primitive")
  expect_identical(type_of(base::list), "primitive")
  expect_identical(type_of(base::eval), "closure")
  expect_identical(type_of(~foo), "formula")
  expect_identical(type_of(quo(foo)), "formula")
  expect_identical(type_of(quote(a := b)), "definition")
  expect_identical(type_of(quote(foo())), "language")
})

test_that("Unicode escapes are always converted to UTF8 characters in as_list()", {
  with_non_utf8_locale({
    env <- child_env(empty_env())
    env_bind(env, !! get_alien_lang_string() := NULL)
    list <- as_list(env)
    expect_identical(names(list), get_alien_lang_string())
  })
})

test_that("no method dispatch", {
  as.logical.foo <- function(x) "wrong"
  expect_identical(as_integer(structure(TRUE, class = "foo")), 1L)

  as.list.foo <- function(x) "wrong"
  expect_identical(as_list(structure(1:10, class = "foo")), as.list(1:10))
})

test_that("input is left intact", {
  x <- structure(TRUE, class = "foo")
  y <- as_integer(x)
  expect_identical(x, structure(TRUE, class = "foo"))
})

test_that("as_list() zaps attributes", {
  expect_identical(as_list(structure(list(), class = "foo")), list())
})

test_that("as_list() only coerces vector or dictionary types", {
  expect_identical(as_list(1:3), list(1L, 2L, 3L))
  expect_error(as_list(quote(symbol)), "a symbol to a list")
})

test_that("as_list() bypasses environment method and leaves input intact", {
  as.list.foo <- function(x) "wrong"
  x <- structure(child_env(NULL), class = "foo")
  y <- as_list(x)

  expect_is(x, "foo")
  expect_identical(y, set_names(list(), character(0)))
})

test_that("as_integer() and as_logical() require integerish input", {
  expect_error(as_integer(1.5), "a fractional double vector to an integer vector")
  expect_error(as_logical(1.5), "a fractional double vector to a logical vector")
})

test_that("names are preserved", {
  nms <- as.character(1:3)
  x <- set_names(1:3, nms)
  expect_identical(names(as_double(x)), nms)
  expect_identical(names(as_list(x)), nms)
})

test_that("as_character() support logical NA", {
  expect_identical(as_character(NA), na_chr)
  expect_identical(as_character(lgl(NA, NA)), c(na_chr, na_chr))
})

test_that("can convert strings (#138)", {
  expect_identical(as_character("a"), "a")
  expect_identical(as_list("a"), list("a"))
})


# --------------------------------------------------------------------

test_that("parse_quosure() forwards to parse_quo()", {
  env <- env()
  expect_identical(parse_quosure("foo", env), parse_quo("foo", env))
  expect_identical(parse_quosures("foo; bar", env), parse_quos("foo; bar", env))
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
  mask <- as_overscope(quo, mtcars)
  expect_true(".__tidyeval_data_mask__." %in% env_names(mask))
})

test_that("overscope functions forward to mask functions", {
  top <- env()
  bottom <- child_env(top, foo = "bar")
  mask <- new_overscope(bottom, top)
  expect_true(env_has(mask, ".__tidyeval_data_mask__."))

  overscope_clean(mask)
  expect_false(env_has(env_parent(mask), "foo"))

  mask <- as_data_mask(mtcars)
  x <- 10
  expect_identical(overscope_eval_next(mask, quote(cyl * x), current_env()), mtcars$cyl * x)
  expect_identical(overscope_eval_next(mask, quote(am * x), current_env()), mtcars$am * x)
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

test_that("node() still works", {
  expect_identical(node(1, NULL), new_node(1, NULL))
})

test_that("set_attrs() fails with uncopyable types", {
  expect_error(set_attrs(env(), foo = "bar"), "is uncopyable")
})

test_that("mut_attrs() fails with copyable types", {
  expect_error(mut_attrs(letters, foo = "bar"), "is copyable")
})

test_that("set_attrs() called with NULL zaps attributes", {
  obj <- set_attrs(letters, foo = "bar")
  expect_identical(set_attrs(obj, NULL), letters)
})

test_that("set_attrs() does not zap old attributes", {
  obj <- set_attrs(letters, foo = "bar")
  obj <- set_attrs(obj, baz = "bam")
  expect_named(attributes(obj), c("foo", "baz"))
})


#  Stack -------------------------------------------------------------

test_that("can standardise call frame", {
  fn <- function(foo = "bar") call_standardise(call_frame())
  expect_identical(fn(), quote(fn()))
  expect_identical(fn("baz"), quote(fn(foo = "baz")))
})

test_that("can modify call frame", {
  fn <- function(foo = "bar") call_modify(call_frame(), baz = "bam", .standardise = TRUE)
  expect_identical(fn(), quote(fn(baz = "bam")))
  expect_identical(fn("foo"), quote(fn(foo = "foo", baz = "bam")))
})

# Beware some sys.x() take `n` and some take `which`

test_that("ctxt_frame() caller agrees with sys.parent()", {
  parent <- sys.parent(n = 1)
  caller <- ctxt_frame()$caller_pos
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

test_that("ctxt_frame(n_depth) returns global frame", {
  n_depth <- ctxt_depth()
  frame <- ctxt_frame(n_depth)
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
  ctxt_frame_messy <- eval(quote(call_frame(clean = FALSE)), new.env())
  expect_identical(ctxt_frame_messy$fn, prim_eval)

  ctxt_frame_clean <- eval(quote(call_frame(clean = TRUE)), new.env())
  expect_identical(ctxt_frame_clean$fn, base::eval)
})

test_that("ctxt_stack_callers() agrees with sys.parents()", {
  parents <- sys.parents()
  callers <- ctxt_stack_callers()
  expect_equal(callers, rev(parents))
})

test_that("ctxt_stack_exprs() agrees with sys.call()", {
  pos <- sys.nframe()
  syscalls <- map(seq(pos, 1), sys.call)
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

test_that("ctxt_stack_fns() returns functions in correct order", {
  f1 <- function(x) f2(x)
  f2 <- function(x) ctxt_stack_fns()
  expect_identical(f1()[1:2], list(f2, f1))
})

test_that("ctxt_stack_fns() handles intervening frames", {
  fns <- ctxt_stack_fns()
  intervened_fns <- identity(identity(ctxt_stack_fns()))
  expect_identical(c(identity, identity, fns), intervened_fns)
})

test_that("ctxt_stack() handles intervening frames", {
  stack <- ctxt_stack()
  intervened_stack <- identity(ctxt_stack())[-1]
  expect_identical(intervened_stack, stack)
})


test_that("call_stack() trail ignores irrelevant frames", {
  f1 <- function(x) f2(x)
  f2 <- function(x) f3()
  f3 <- function(x) call_stack()

  stack1 <- f1()
  trail1 <- pluck_int(stack1, "pos")
  expect_equal(fixup_call_trail(trail1), c(3, 2, 1))

  stack2 <- identity(identity(f1()))
  trail2 <- pluck_int(stack2, "pos")
  expect_equal(fixup_call_trail(trail2), c(5, 4, 3))
})

test_that("ctxt_stack() exprs is in opposite order to sys calls", {
  syscalls <- sys.calls()
  stack <- ctxt_stack()
  stack <- drop_last(stack) # global frame
  exprs <- pluck(stack, "expr")
  expect_equal(exprs[[length(exprs)]], syscalls[[1]])
  expect_equal(exprs[[1]], syscalls[[length(syscalls)]])
})

test_that("ctxt_stack() and call_stack() agree", {
  call_stack <- call_stack()
  call_stack <- drop_last(call_stack) # global frame
  positions <- map_int(call_stack, `[[`, "pos")

  ctxt_stack <- ctxt_stack()
  ctxt_stack <- drop_last(ctxt_stack) # global frame
  ctxt_stack <- rev(ctxt_stack)[positions]

  call_exprs <- map(call_stack, `[[`, "expr")
  eval_exprs <- map(ctxt_stack, `[[`, "expr")
  expect_identical(call_exprs, eval_exprs)

  is_eval <- map_lgl(call_stack, function(frame) {
    identical(frame$fn, base::eval)
  })

  call_envs <- map(call_stack[!is_eval], `[[`, "env")
  eval_envs <- map(ctxt_stack[!is_eval], `[[`, "env")
  expect_identical(call_envs, eval_envs)
})

test_that("ctxt_stack() subsets n frames", {
  stack <- ctxt_stack()
  stack_2 <- ctxt_stack(2)
  expect_identical(stack_2, stack[1:2])

  n <- ctxt_depth()
  stack_n <- ctxt_stack(n)
  expect_identical(stack_n, stack)

  # Get correct eval depth within expect_error()
  expect_error({ n <- ctxt_depth(); stop() })
  expect_error(ctxt_stack(n + 1), "not that many frames")
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

test_that("ctxt_stack() trims layers of calls", {
  current_stack <- ctxt_stack()
  expect_identical(identity(identity(ctxt_stack(trim = 1))), current_stack)

  fn <- function(trim) identity(identity(ctxt_stack(trim = trim)))
  stack <- identity(identity(fn(2)))
  expect_identical(stack, current_stack)
})

test_that("frame_position() returns correct position", {
  fn <- function() {
    env <- environment()
    pos <- ctxt_frame()$pos
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
    frame <- ctxt_frame(distance)
    expect_identical(frame$env, env)

    burried_distance <- identity(frame_position(env, from = "current"))
    expect_equal(distance, burried_distance)
  }
  fn()
})

test_that("evaluation stack is trimmed from layers of calls", {
  stack <- ctxt_stack()
  trimmed_stack <- identity(stack_trim(identity(ctxt_stack())))
  expect_identical(stack, trimmed_stack)
})

test_that("call is not modified in place", {
  f <- function(...) g(...)
  g <- function(...) call_stack()[1:2]
  stack <- f(foo)
  expect_equal(stack[[1]]$expr, quote(g(...)))
})

test_that("finds correct env type - frame", {
  expect_identical(identity(env_type(ctxt_frame(2)$env)), "frame")
})

test_that("retired _len() ctors still work", {
  scoped_options(lifecycle_verbose_soft_deprecation = FALSE)
  expect_identical(lgl_len(2), new_logical(2))
  expect_identical(int_len(2), new_integer(2))
  expect_identical(dbl_len(2), new_double(2))
  expect_identical(chr_len(2), new_character(2))
  expect_identical(cpl_len(2), new_complex(2))
  expect_identical(raw_len(2), new_raw(2))
  expect_identical(bytes_len(2), new_raw(2))
  expect_identical(list_len(2), new_list(2))
})

test_that("retired _along() ctors still work", {
  scoped_options(lifecycle_verbose_soft_deprecation = FALSE)
  expect_identical(lgl_along(1:2), new_logical_along(1:2))
  expect_identical(int_along(1:2), new_integer_along(1:2))
  expect_identical(dbl_along(1:2), new_double_along(1:2))
  expect_identical(chr_along(1:2), new_character_along(1:2))
  expect_identical(cpl_along(1:2), new_complex_along(1:2))
  expect_identical(raw_along(1:2), new_raw_along(1:2))
  expect_identical(bytes_along(1:2), new_raw_along(1:2))
  expect_identical(list_along(1:2), new_list_along(1:2))
})

test_that("whole scope is purged", {
  scoped_options(lifecycle_verbose_soft_deprecation = FALSE)

  outside <- child_env(NULL, important = TRUE)
  top <- child_env(outside, foo = "bar", hunoz = 1)
  mid <- child_env(top, bar = "baz", hunoz = 2)

  data_mask_objects <- list(
    .top_env = top,
    .env = 1,
    `~` = 2,
    .__tidyeval_data_mask__. = env()
  )
  bottom <- child_env(mid, !!! data_mask_objects)

  overscope_clean(bottom)

  expect_identical(names(bottom), character(0))
  expect_identical(names(mid), character(0))
  expect_identical(names(top), character(0))
  expect_identical(names(outside), "important")
})

test_that("pattern match on string encoding", {
  expect_defunct(is_character(letters, encoding = "unknown"))
})

test_that("vector _along() ctors pick up names", {
  x <- list(a = NULL, b = NULL)
  expect_identical(new_logical_along(x), c(a = NA, b = NA))
  expect_identical(new_integer_along(x), c(a = na_int, b = na_int))
  expect_identical(new_double_along(x), c(a = na_dbl, b = na_dbl))
  expect_identical(new_complex_along(x), c(a = na_cpl, b = na_cpl))
  expect_identical(new_character_along(x), c(a = na_chr, b = na_chr))
  expect_identical(new_raw_along(x), set_names(raw(2), c("a", "b")))
  expect_identical(new_list_along(x), list(a = NULL, b = NULL))
})

test_that("vector _along() ctors pick up names", {
  x <- list(a = NULL, b = NULL)
  expect_identical(new_logical_along(x, toupper), c(A = NA, B = NA))
  expect_identical(new_integer_along(x, toupper), c(A = na_int, B = na_int))
  expect_identical(new_double_along(x, toupper), c(A = na_dbl, B = na_dbl))
  expect_identical(new_complex_along(x, toupper), c(A = na_cpl, B = na_cpl))
  expect_identical(new_character_along(x, toupper), c(A = na_chr, B = na_chr))
  expect_identical(new_raw_along(x, toupper), set_names(raw(2), c("A", "B")))
  expect_identical(new_list_along(x, toupper), list(A = NULL, B = NULL))
})

test_that("vector is modified", {
  x <- c(1, b = 2, c = 3, 4)
  out <- modify(x, 5, b = 20, splice(list(6, c = "30")))
  expect_equal(out, list(1, b = 20, c = "30", 4, 5, 6))
})

test_that("invoke() buries arguments", {
  expect_identical(invoke(call_inspect, 1:2, 3L), quote(.fn(`1`, `2`, `3`)))
  expect_identical(invoke("call_inspect", 1:2, 3L), quote(call_inspect(`1`, `2`, `3`)))
  expect_identical(invoke(call_inspect, 1:2, 3L, .bury = c("foo", "bar")), quote(foo(`bar1`, `bar2`, `bar3`)))
  expect_identical(invoke(call_inspect, 1:2, 3L, .bury = NULL), as.call(list(call_inspect, 1L, 2L, 3L)))
})

test_that("invoke() can be called without arguments", {
  expect_identical(invoke("list"), list())
  expect_identical(invoke(list), list())
})

test_that("quo_expr() still works", {
  x <- quo(foo(!!quo(bar), !!local(quo(baz))))
  expect_identical(quo_expr(x), quo_squash(x))
})
