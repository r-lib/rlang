context("trace.R")

# These tests must come first because print method includes srcrefs
test_that("tree printing only changes deliberately", {
  skip_unless_utf8()

  scoped_options(
    rlang_trace_format_srcrefs = TRUE,
    rlang_trace__force_dangling_srcrefs = TRUE
  )

  dir <- normalizePath(test_path(".."))
  e <- environment()

  i <- function(i) j(i)
  j <- function(i) { k(i) }
  k <- function(i) {
    NULL
    l(i)
  }
  l <- function(i) trace_back(e)
  trace <- i()

  expect_known_output(file = test_path("test-trace-print.txt"), {
    print(trace, dir = dir)
    cat("\n")
    print(trace_subset(trace, 0L), dir = dir)
  })
})

test_that("can print tree with collapsed branches", {
  skip_unless_utf8()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  scoped_options(
    rlang_trace_format_srcrefs = TRUE,
    rlang_trace__force_dangling_srcrefs = TRUE
  )

  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() { g() }
  g <- function() { tryCatch(h(), foo = identity, bar = identity) }
  h <- function() { tryCatch(i(), baz = identity) }
  i <- function() { tryCatch(trace_back(e, bottom = 0)) }
  trace <- eval(quote(f()))

  expect_known_trace_output(trace,
    file = test_path("test-trace-collapsed1.txt"),
    dir = dir,
    srcrefs = TRUE
  )

  # With multiple siblings
  f <- function() eval(quote(eval(quote(g()))))
  g <- function() tryCatch(eval(quote(h())), foo = identity, bar = identity)
  h <- function() trace_back(e)
  trace <- eval(quote(f()))

  expect_known_trace_output(trace,
    file = test_path("test-trace-collapsed2.txt"),
    dir = dir,
    srcrefs = TRUE
  )
})

test_that("trace_simplify_branch() extracts last branch", {
  e <- environment()
  j <- function(i) k(i)
  k <- function(i) l(i)
  l <- function(i) eval(quote(m()), parent.frame(i))
  m <- function() trace_back(e)

  x1 <- j(1)
  expect_trace_length(x1, 6)
  expect_trace_length(trace_simplify_branch(x1), 3)

  x2 <- j(2)
  expect_trace_length(x2, 6)
  expect_trace_length(trace_simplify_branch(x2), 2)

  x3 <- j(3)
  expect_trace_length(x2, 6)
  expect_trace_length(trace_simplify_branch(x3), 1)
})

test_that("integerish indices are allowed", {
  trace <- trace_back()
  expect_identical(trace_subset(trace, 0), trace_subset(trace, 0L))
})

test_that("cli_branch() handles edge case", {
  e <- environment()
  f <- function() trace_back(e)
  trace <- f()

  call <- paste0(" ", cli_style$h, "rlang:::f()")
  tree <- trace_as_tree(trace, srcrefs = FALSE)
  expect_identical(cli_branch(tree$call[-1], trace$indices), call)
})

test_that("trace formatting picks up `rlang_trace_format_srcrefs`", {
  scoped_options(rlang_trace__force_dangling_srcrefs = TRUE)

  e <- environment()
  f <- function() trace_back(e)
  trace <- f()

  with_options(
    rlang_trace_format_srcrefs = FALSE,
    expect_false(any(grepl("testthat", format(trace))))
  )
  with_options(
    rlang_trace_format_srcrefs = TRUE,
    expect_true(any(!!grepl("test-trace\\.R", format(trace))))
  )
})

test_that("trace picks up option `rlang_trace_top_env` for trimming trace", {
  e <- current_env()
  f1 <- function() trace_back()
  f2 <- function() trace_back(e)
  with_options(rlang_trace_top_env = current_env(),
    expect_identical(trace_length(f1()), trace_length(f2()))
  )
})

test_that("collapsed formatting doesn't collapse single frame siblings", {
  e <- current_env()
  f <- function() eval_bare(quote(g()))
  g <- function() trace_back(e)
  trace <- f()

  full <- capture.output(print(trace, simplify = "none", srcrefs = FALSE))[[3]]
  expect_match(full, "rlang::eval_bare(quote(g()))", fixed = TRUE)

  collapsed <- capture.output(print(trace, simplify = "collapse", srcrefs = FALSE))[[3]]
  expect_match(collapsed, "[ rlang::eval_bare(...) ]", fixed = TRUE)
})

test_that("recursive frames are rewired to the global env", {
  skip_unless_utf8()

  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() g()
  g <- function() trace_back(e)
  trace <- eval_tidy(quo(f()))

  expect_known_trace_output(trace, file = "test-trace-recursive.txt")
})

test_that("long backtrace branches are truncated", {
  skip_unless_utf8()

  e <- current_env()
  f <- function(n) {
    if (n) {
      return(f(n - 1))
    }
    trace_back(e)
  }
  trace <- f(10)

  expect_known_output(file = test_path("test-trace-truncate-backtrace-branch.txt"), {
    cat("Full:\n")
    print(trace, simplify = "branch", srcrefs = FALSE)
    cat("\n5 frames:\n")
    print(trace, simplify = "branch", max_frames = 5, srcrefs = FALSE)
    cat("\n2 frames:\n")
    print(trace, simplify = "branch", max_frames = 2, srcrefs = FALSE)
    cat("\n1 frame:\n")
    print(trace, simplify = "branch", max_frames = 1, srcrefs = FALSE)
  })

  expect_error(print(trace, simplify = "none", max_frames = 5), "currently only supported with")
  expect_error(print(trace, simplify = "collapse", max_frames = 5), "currently only supported with")
})

test_that("eval() frames are collapsed", {
  skip_unless_utf8()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  e <- current_env()
  f <- function() base::eval(quote(g()))
  g <- function() eval(quote(trace_back(e, bottom = 0)))
  trace <- f()

  expect_known_trace_output(trace, file = "test-trace-collapse-eval.txt")

  f <- function() base::evalq(g())
  g <- function() evalq(trace_back(e, bottom = 0))
  trace <- f()

  expect_known_trace_output(trace, file = "test-trace-collapse-evalq.txt")
})

test_that("%>% frames are collapsed", {
  skip_unless_utf8()
  skip_if_not_installed("magrittr")

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  f <- function(x, ...) x
  g <- function(x, ...) x
  h <- function(x, ...) trace_back(e)

  trace <- NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
  expect_known_trace_output(trace, "test-trace-collapse-magrittr.txt")

  trace <- f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
  expect_known_trace_output(trace, "test-trace-collapse-magrittr2.txt")

  trace <- f(g(NULL %>% f()) %>% h())
  expect_known_trace_output(trace, "test-trace-collapse-magrittr3.txt")
})

test_that("children of collapsed %>% frames have correct parent", {
  skip_unless_utf8()
  skip_if_not_installed("magrittr")

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  F <- function(x, ...) x
  G <- function(x, ...) x
  H <- function(x) f()
  f <- function() h()
  h <- function() trace_back(e)

  trace <- NA %>% F() %>% G() %>% H()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-children.txt")
})

test_that("children of collapsed frames are rechained to correct parent", {
  skip_unless_utf8()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  e <- current_env()
  f <- function() eval(quote(g()), env())
  g <- function() trace_back(e)
  trace <- f()

  expect_known_output(file = test_path("test-trace-collapse-children.txt"), {
    cat("Full:\n")
    print(trace, simplify = "none", srcrefs = FALSE)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", srcrefs = FALSE)
    cat("\nBranch:\n")
    print(trace, simplify = "branch", srcrefs = FALSE)
  })
})

test_that("pipe_collect_calls() collects calls", {
  exprs2 <- function(...) unname(exprs(...))
  placeholder <- function() NULL

  call <- quote(a(A %>% B) %>% b)
  out <- pipe_collect_calls(call, placeholder)
  expect_identical(out$calls, exprs2(rlang:::a(A %>% B), rlang:::b(.)))
  expect_true(out$leading)

  call <- quote(a %>% b %>% c)
  out <- pipe_collect_calls(call, placeholder)
  expect_identical(out$calls, exprs2(rlang:::b(.), rlang:::c(.)))
  expect_false(out$leading)

  call <- quote(a() %>% b %>% c)
  out <- pipe_collect_calls(call, placeholder)
  expect_identical(out$calls, exprs2(rlang:::a(), rlang:::b(.), rlang:::c(.)))
  expect_true(out$leading)
})

test_that("combinations of incomplete and leading pipes collapse properly", {
  skip_unless_utf8()
  skip_if_not_installed("magrittr")

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  F <- function(x, ...) x
  T <- function(x) trace_back(e)

  trace <- NA %>% F() %>% T() %>% F() %>% F()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-incomplete.txt")

  trace <- T(NA) %>% F()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-incomplete-leading1.txt")

  trace <- F(NA) %>% F() %>% T() %>% F() %>% F()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-incomplete-leading2.txt")

  trace <- NA %>% T()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-complete1.txt")

  trace <- NA %>% F() %>% T()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-complete2.txt")

  trace <- F(NA) %>% T()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-complete-leading1.txt")

  trace <- F(NA) %>% F() %>%  T()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-complete-leading2.txt")
})

test_that("calls before and after pipe are preserved", {
  skip_unless_utf8()
  skip_if_not_installed("magrittr")

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  F <- function(x, ...) x
  T <- function(x) trace_back(e)
  C <- function(x) f()
  f <- function() trace_back(e)

  trace <- F(NA %>% T())
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-before-after1.txt")

  trace <- NA %>% C()
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-before-after2.txt")

  trace <- F(NA %>% C())
  expect_known_trace_output(trace, "test-trace-collapse-magrittr-before-after3.txt")
})

test_that("always keep very first frame as part of backtrace branch", {
  skip_unless_utf8()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  e <- current_env()

  gen <- function(x) UseMethod("gen")
  gen.default <- function(x) trace_back(e)

  trace <- gen()
  expect_known_trace_output(trace, "test-trace-backtrace-branch-first-frame.txt")
})

test_that("can take the str() of a trace (#615)", {
  e <- current_env()
  f <- function(n) if (n < 10) f(n - 1) else trace_back(e)
  expect_output(expect_no_error(str(f(10))))
})

test_that("anonymous calls are stripped from backtraces", {
  skip_unless_utf8()

  e <- current_env()
  trace <- (function() {
    "foo"
    "bar"
    trace_back(e)
  })()

  expect_identical(format(trace, simplify = "branch"), chr())
  expect_known_trace_output(trace, "test-trace-backtrace-anonymous.txt")
})

test_that("collapsing of eval() frames detects when error occurs within eval()", {
  skip_unless_utf8()

  e <- NULL
  trace <- NULL

  fn <- function() {
    scoped_options(
      rlang_trace_format_srcrefs = FALSE
    )
    e <<- current_env()
    eval()
  }

  catch_cnd(with_handlers(
    fn(),
    error = calling(function(err) trace <<- trace_back(e))
  ))

  expect_known_trace_output(trace, "test-trace-non-collapsed-eval")
})

test_that("can print degenerate backtraces", {
  skip_unless_utf8()

  trace_sym <- new_trace(list(quote(foo)), int(0), chr(""))
  expect_known_trace_output(trace_sym, file = "test-trace-degenerate-sym.txt")

  trace_null <- new_trace(list(NULL), int(0), chr(""))
  expect_known_trace_output(trace_null, file = "test-trace-degenerate-null.txt")

  trace_scalar <- new_trace(list(1L), int(0), chr(""))
  expect_known_trace_output(trace_scalar, file = "test-trace-degenerate-scalar.txt")
})

test_that("check for dangling promise in call CAR (#492)", {
  skip_unless_utf8()

  expect_known_trace_output(file = "test-trace-call-car-promise.txt", local({
    e <- current_env()

    print.foo <- function(x) {
      rlang::trace_back(e)
    }

    foo <- structure(list(), class = "foo")
    print(foo)
  }))
})

test_that("dangling srcrefs are not printed", {
  skip_unless_utf8()

  from <- test_path("fixtures", "trace-srcref.R")
  to <- test_path("fixtures", "trace-srcref2.R")

  file.copy(from, to)
  on.exit(unlink(to))

  source(to, local = TRUE, keep.source = TRUE)
  unlink(to)

  expect_known_trace_output(
    local(f(current_env())),
    file ="test-trace-dangling-srcref.txt",
    srcrefs = TRUE
  )
})

test_that("summary.rlang_trace() prints the full tree", {
  skip_unless_utf8()

  e <- current_env()
  f <- function() g()
  g <- function() h()
  h <- function() trace_back(e)
  trace <- f()
  expect_known_output(summary(trace, srcrefs = FALSE), file = test_path("test-trace-summary.txt"))
})

test_that("unexported functions have `:::` prefix", {
  skip_unless_utf8()

  # Should be installed as part of the C API tests
  skip_if_not_installed("rlanglibtest")
  test_trace_unexported_child <- env_get(ns_env("rlanglibtest"), "test_trace_unexported_child")

  e <- current_env()
  f <- function() test_trace_unexported_child(e)
  trace <- f()

  expect_known_trace_output(trace, file = "test-trace-unexported-prefix.txt")
})

test_that("global functions have `global::` prefix", {
  skip_unless_utf8()

  f <- eval_bare(expr(function(e) rlang::trace_back(e)), global_env())
  g <- function(e) f(e)
  trace <- g(current_env())

  expect_known_trace_output(trace, file = "test-trace-global-prefix.txt")
})

test_that("local functions inheriting from global do not have `global::` prefix", {
  skip_unless_utf8()

  f <- eval_bare(expr(function(e) rlang::trace_back(e)), env(global_env()))
  g <- function(e) f(e)
  trace <- g(current_env())

  expect_known_trace_output(trace, file = "test-trace-local-prefix.txt")
})

test_that("can trim layers of backtraces", {
  skip_unless_utf8()

  e <- current_env()
  f <- function(n) identity(identity(g(n)))
  g <- function(n) identity(identity(h(n)))
  h <- function(n) identity(identity(trace_back(e, bottom = n)))

  trace0 <- f(0)
  trace1 <- f(1)
  trace2 <- f(2)
  trace3 <- f(3)

  expect_known_output(file = test_path("test-trace-trim.txt"), {
    scoped_options(rlang_trace_format_srcrefs = FALSE)

    cat_line("No trimming:")
    summary(trace0)

    cat_line("", "", "One layer (the default):")
    summary(trace1)

    cat_line("", "", "Two layers:")
    summary(trace2)

    cat_line("", "", "Three layers:")
    summary(trace3)
  })

  # Test that trimming with frame environment is equivalent
  e <- current_env()
  f <- function(n) identity(identity(g(n)))
  g <- function(n) identity(identity(h(n)))
  h <- function(n) identity(identity(trace_back(e, bottom = caller_env(n - 1L))))

  trace1_env <- f(1)
  trace2_env <- f(2)
  trace3_env <- f(3)

  expect_equal_trace(trace1, trace1_env)
  expect_equal_trace(trace2, trace2_env)
  expect_equal_trace(trace3, trace3_env)
})

test_that("can subset from bottom tree level", {
  idx <- int(0, 1, 1, 1)
  expect_identical(chain_indices(4L, idx), c(1L, 4L))
  expect_identical(parents_indices(4L, idx), c(1L, 4L))
  expect_identical(children_indices(4L, idx), int())

  e <- current_env()
  f <- function() identity(identity(g()))
  g <- function() trace_back(e)
  trace <- f()
  out <- trace_subset_across(trace, 3)

  f <- function() g()
  expected <- f()

  expect_equal_trace(out, expected)
})

test_that("can subset from top tree level", {
  idx <- int(0, 1, 0, 3, 3, 3)
  expect_identical(chain_indices(3L, idx), 3:6)
  expect_identical(parents_indices(3L, idx), 3L)
  expect_identical(children_indices(3L, idx), 4:6)

  e <- current_env()
  f <- function() identity(identity(g()))
  g <- function() trace_back(e)
  trace <- tryCatch(f())

  out <- trace_subset_across(trace, -1, 1)
  expected <- f()

  expect_equal_trace(out, expected)
})

test_that("can subset in middle level", {
  idx <- int(0, 1, 1, 1, 4, 4, 4)
  expect_identical(chain_indices(3L, idx), c(1L, 3L))
  expect_identical(parents_indices(3L, idx), c(1L, 3L))
  expect_identical(children_indices(3L, idx), int())

  e <- current_env()
  f <- function() identity(identity(g()))
  g <- function() identity(identity(h()))
  h <- function() trace_back(e)
  trace <- f()

  out <- trace_subset_across(trace, 2, 2)
  expect_equal(out$calls, alist(rlang:::f(), base::identity(g())))
  expect_identical(out$parents, 0:1)


  idx <- int(0, 1, 1, 1, 4, 4, 4)
  expect_identical(chain_indices(4L, idx), c(1L, 4:7))
  expect_identical(parents_indices(4L, idx), c(1L, 4L))
  expect_identical(children_indices(4L, idx), 5:7)

  out <- trace_subset_across(trace, 3, 2)
  exp <- alist(
    rlang:::f(),
    rlang:::g(),
    base::identity(identity(h())),
    base::identity(h()),
    rlang:::h()
  )
  expect_equal(out$calls, exp)
  expect_identical(out$parents, c(0L, 1L, 2L, 2L, 2L))
})

test_that("fails when `bottom` is not on the stack", {
  expect_error(trace_back(bottom = env()), "Can't find `bottom`")
})

test_that("caught error does not display backtrace in knitted files", {
  skip_if(!rmarkdown::pandoc_available())

  scoped_options(
    rlang_backtrace_on_error = NULL,
    rlang_interactive = FALSE
  )
  lines <- render_md("test-trace.Rmd")
  error_line <- lines[[length(lines)]]
  expect_match(error_line, "foo$")
})
