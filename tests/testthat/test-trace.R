local_options(
  rlang_trace_use_winch = FALSE
)

# These tests must come first because print method includes srcrefs
test_that("tree printing only changes deliberately", {
  # Because of srcrefs
  skip_on_cran()
  skip_if_not_installed("testthat", "2.99.0")

  local_options(
    rlang_trace_format_srcrefs = TRUE
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

  expect_snapshot({
    print(trace, dir = dir)
    cat("\n")
    print(trace_slice(trace, 0L), dir = dir)
  })
})

test_that("can print tree with collapsed branches", {
  # Because of srcrefs
  skip_on_cran()
  skip_if_not_installed("testthat", "2.99.0")

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  local_options(
    rlang_trace_format_srcrefs = TRUE
  )

  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() { g() }
  g <- function() { tryCatch(h(), foo = identity, bar = identity) }
  h <- function() { tryCatch(i(), baz = identity) }
  i <- function() { tryCatch(trace_back(e, bottom = 0)) }
  trace <- eval(quote(f()))

  expect_snapshot_trace(trace,
    dir = dir,
    srcrefs = TRUE
  )

  # With multiple siblings
  f <- function() eval(quote(eval(quote(g()))))
  g <- function() tryCatch(eval(quote(h())), foo = identity, bar = identity)
  h <- function() trace_back(e)
  trace <- eval(quote(f()))

  expect_snapshot_trace(trace,
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
  expect_equal(sum(x1$visible), 6)
  expect_equal(sum(trace_simplify_branch(x1)$visible), 3)

  x2 <- j(2)
  expect_equal(sum(x2$visible), 6)
  expect_equal(sum(trace_simplify_branch(x2)$visible), 2)

  x3 <- j(3)
  expect_equal(sum(x3$visible), 1)
  expect_equal(sum(trace_simplify_branch(x3)$visible), 1)
})

test_that("integerish indices are allowed", {
  trace <- trace_back()
  expect_identical(trace_slice(trace, 0), trace_slice(trace, 0L))
})

test_that("cli_branch() handles edge case", {
  e <- environment()
  f <- function() trace_back(e)
  trace <- f()

  tree <- trace_as_tree(trace, srcrefs = FALSE)
  expect_snapshot(cli_branch(tree[-1, ]))
})

test_that("trace formatting picks up `rlang_trace_format_srcrefs`", {
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

  expect_snapshot({
    print(trace, simplify = "none", srcrefs = FALSE)
    print(trace, simplify = "collapse", srcrefs = FALSE)
  })
})

test_that("recursive frames are rewired to the global env", {
  dir <- normalizePath(test_path(".."))
  e <- environment()

  f <- function() g()
  g <- function() trace_back(e)
  trace <- eval_tidy(quo(f()))

  expect_snapshot_trace(trace)
})

test_that("long backtrace branches are truncated", {
  e <- current_env()
  f <- function(n) {
    if (n) {
      return(f(n - 1))
    }
    trace_back(e)
  }
  trace <- f(10)

  expect_snapshot({
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
  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  e <- current_env()
  f <- function() base::eval(quote(g()))
  g <- function() eval(quote(trace_back(e, bottom = 0)))
  trace <- f()

  expect_snapshot_trace(trace)

  f <- function() base::evalq(g())
  g <- function() evalq(trace_back(e, bottom = 0))
  trace <- f()

  expect_snapshot_trace(trace)
})

test_that("%>% frames are collapsed", {
  skip_if_not_installed("magrittr", "1.5.0.9000")
  skip_on_cran()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  f <- function(x, ...) x
  g <- function(x, ...) x
  h <- function(x, ...) trace_back(e)

  trace <- NULL %>% f() %>% g(1, 2) %>% h(3, ., 4)
  expect_snapshot_trace(trace)

  trace <- f(NULL) %>% g(list(.)) %>% h(3, ., list(.))
  expect_snapshot_trace(trace)

  trace <- f(g(NULL %>% f()) %>% h())
  expect_snapshot_trace(trace)
})

test_that("children of collapsed %>% frames have correct parent", {
  skip_if_not_installed("magrittr", "1.5.0.9000")
  skip_on_cran()

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
  expect_snapshot_trace(trace)
})

test_that("children of collapsed frames are rechained to correct parent", {
  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  e <- current_env()
  f <- function() eval(quote(g()), env())
  g <- function() trace_back(e)
  trace <- f()

  expect_snapshot({
    cat("Full:\n")
    print(trace, simplify = "none", srcrefs = FALSE)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", srcrefs = FALSE)
    cat("\nBranch:\n")
    print(trace, simplify = "branch", srcrefs = FALSE)
  })
})

test_that("combinations of incomplete and leading pipes collapse properly", {
  skip_if_not_installed("magrittr", "1.5.0.9000")
  skip_on_cran()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  F <- function(x, ...) x
  T <- function(x) trace_back(e)

  trace <- NA %>% F() %>% T() %>% F() %>% F()
  expect_snapshot_trace(trace)

  trace <- T(NA) %>% F()
  expect_snapshot_trace(trace)

  trace <- F(NA) %>% F() %>% T() %>% F() %>% F()
  expect_snapshot_trace(trace)

  trace <- NA %>% T()
  expect_snapshot_trace(trace)

  trace <- NA %>% F() %>% T()
  expect_snapshot_trace(trace)

  trace <- F(NA) %>% T()
  expect_snapshot_trace(trace)

  trace <- F(NA) %>% F() %>%  T()
  expect_snapshot_trace(trace)
})

test_that("calls before and after pipe are preserved", {
  skip_if_not_installed("magrittr", "1.5.0.9000")
  skip_on_cran()

  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  `%>%` <- magrittr::`%>%`

  e <- current_env()
  F <- function(x, ...) x
  T <- function(x) trace_back(e)
  C <- function(x) f()
  f <- function() trace_back(e)

  trace <- F(NA %>% T())
  expect_snapshot_trace(trace)

  trace <- NA %>% C()
  expect_snapshot_trace(trace)

  trace <- F(NA %>% C())
  expect_snapshot_trace(trace)
})

test_that("always keep very first frame as part of backtrace branch", {
  # Fake eval() call does not have same signature on old R
  skip_if(getRversion() < "3.4")

  e <- current_env()

  gen <- function(x) UseMethod("gen")
  gen.default <- function(x) trace_back(e)

  trace <- gen()
  expect_snapshot_trace(trace)
})

test_that("can take the str() of a trace (#615)", {
  e <- current_env()
  f <- function(n) if (n < 10) f(n - 1) else trace_back(e)
  expect_output(expect_no_error(str(f(10))))
})

test_that("anonymous calls are stripped from backtraces", {
  e <- current_env()
  trace <- (function() {
    "foo"
    "bar"
    trace_back(e)
  })()

  expect_identical(format(trace, simplify = "branch"), chr())
  expect_snapshot_trace(trace)
})

test_that("collapsing of eval() frames detects when error occurs within eval()", {
  e <- NULL
  trace <- NULL

  fn <- function() {
    local_options(
      rlang_trace_format_srcrefs = FALSE
    )
    e <<- current_env()
    eval()
  }

  catch_cnd(with_handlers(
    fn(),
    error = calling(function(err) trace <<- trace_back(e))
  ))

  expect_snapshot_trace(trace)
})

test_that("can print degenerate backtraces", {
  trace_sym <- new_trace(list(quote(foo)), int(0))
  expect_snapshot_trace(trace_sym)

  trace_null <- new_trace(list(NULL), int(0))
  expect_snapshot_trace(trace_null)

  trace_scalar <- new_trace(list(1L), int(0))
  expect_snapshot_trace(trace_scalar)
})

test_that("check for dangling promise in call CAR (#492)", {
  expect_snapshot_trace(local({
    e <- current_env()

    print.foo <- function(x) {
      rlang::trace_back(e)
    }

    foo <- structure(list(), class = "foo")
    print(foo)
  }))
})

test_that("dangling srcrefs are not printed", {
  from <- test_path("fixtures", "trace-srcref.R")
  to <- test_path("fixtures", "trace-srcref2.R")

  file.copy(from, to)
  on.exit(unlink(to))

  source(to, local = TRUE, keep.source = TRUE)
  unlink(to)

  expect_snapshot_trace(
    local(f(current_env())),
    srcrefs = TRUE
  )
})

test_that("summary.rlang_trace() prints the full tree", {
  e <- current_env()
  f <- function() g()
  g <- function() h()
  h <- function() trace_back(e)
  trace <- f()
  expect_snapshot(summary(trace, srcrefs = FALSE))
})

test_that("unexported functions have `:::` prefix", {
  expect_true(TRUE)
  return("no longer using the rlanglibtest")

  # Should be installed as part of the C API tests
  skip_if_not_installed("rlanglibtest")
  test_trace_unexported_child <- env_get(ns_env("rlanglibtest"), "test_trace_unexported_child")

  e <- current_env()
  f <- function() test_trace_unexported_child(e)
  trace <- f()

  expect_snapshot_trace(trace)
})

test_that("global functions have `global::` prefix", {
  f <- eval_bare(expr(function(e) rlang::trace_back(e)), global_env())
  g <- function(e) f(e)
  trace <- g(current_env())

  expect_snapshot_trace(trace)
})

test_that("local functions inheriting from global do not have `global::` prefix", {
  f <- eval_bare(expr(function(e) rlang::trace_back(e)), env(global_env()))
  g <- function(e) f(e)
  trace <- g(current_env())

  expect_snapshot_trace(trace)
})

test_that("can trim layers of backtraces", {
  e <- current_env()
  f <- function(n) identity(identity(g(n)))
  g <- function(n) identity(identity(h(n)))
  h <- function(n) identity(identity(trace_back(e, bottom = n)))

  trace0 <- f(0)
  trace1 <- f(1)
  trace2 <- f(2)
  trace3 <- f(3)

  expect_snapshot({
    local_options(rlang_trace_format_srcrefs = FALSE)

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

test_that("fails when `bottom` is not on the stack", {
  expect_error(trace_back(bottom = env()), "Can't find `bottom`")
})

test_that("caught error does not display backtrace in knitted files", {
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")
  skip_if(!rmarkdown::pandoc_available())

  local_options(
    rlang_backtrace_on_error = NULL,
    rlang_backtrace_on_error_report = NULL,
    rlang_interactive = FALSE
  )

  lines <- render_md("test-trace.Rmd")
  error_line <- lines[[length(lines)]]
  expect_match(error_line, "foo$")
  
  expect_snapshot({
    cat_line(render_md("test-trace-full.Rmd"))
  })
})

test_that("empty backtraces are dealt with", {
  foo <- NULL

  local({
    env <- new.env()
    local_options(rlang_trace_top_env = env)
    tryCatch(
      error = identity,
      withCallingHandlers(
        error = function(cnd) foo <<- cnd_entrace(cnd),
        eval(quote(stop("stop")), env)
      )
    )
  })

  expect_identical(trace_length(foo$trace), 0L)
})

test_that("can trace back with quosured symbol", {
  e <- current_env()
  f <- function(foo = g()) {
    # This will create a call in the call stack that isn't really a call
    quo <- quo(foo)

    # Quosure must be nested otherwise `eval_tidy()` unwraps it
    eval_tidy(expr(identity(!!quo)))
  }
  g <- function() trace_back(e)

  # FIXME: Weird trace structure
  trace <- f()
  expect_s3_class(trace, "rlang_trace")
})

test_that("can slice backtrace", {
  trace <- new_trace(alist(a(), b(), c()), 0:2)

  expect_identical(
    trace_slice(trace, 2:3),
    new_trace(alist(b(), c()), 0:1)
  )

  exp <- new_trace(alist(a(), c()), c(0L, 0L))
  
  expect_identical(
    trace_slice(trace, c(1, 3)),
    exp
  )
  expect_identical(
    trace_slice(trace, -2),
    exp
  )
})

test_that("backtraces carry `version` attribute", {
  expect_identical(attr(trace_back(), "version"), 2L)
})

test_that("can bind backtraces", {
  trace1 <- new_trace(alist(a(), b(), c()), 0:2)

  expect_equal(trace_bind(), new_trace(list(), int()))
  expect_equal(trace_bind(trace1), trace1)
  
  trace2 <- new_trace(alist(foo(), bar(), baz()), c(0L, 1L, 1L))
  out <- trace_bind(trace1, trace2)

  expect_equal(
    out$call,
    alist(a(), b(), c(), foo(), bar(), baz())
  )

  expect_equal(
    out$parent,
    c(0:3, c(4L, 4L))
  )
})

test_that("backtraces don't contain inlined objects (#1069, r-lib/testthat#1223)", {
  # !! deparsing in older R
  skip_if_not_installed("base", "3.5.0")

  local_options(
    rlang_trace_format_srcrefs = FALSE
  )

  e <- environment()
  f <- function(...) do.call("g", list(runif(1e6) + 0))
  g <- function(...) h()
  h <- function() trace_back(e)
  trace <- inject(f(!!list()))

  expect_snapshot(summary(trace))
  expect_lt(object.size(trace$call), 50000)
})

test_that("runs of namespaces are embolden (#946)", {
  local_options(
    rlang_trace_format_srcrefs = FALSE,
    rlang_trace_top_env = current_env()
  )
  f <- function() g()
  g <- function() h()
  h <- function() identity(1 + "")
  err <- catch_cnd(withCallingHandlers(f(), error = entrace), "error")

  testthat::local_reproducible_output(crayon = TRUE)

  expect_snapshot({
    print(err)
    summary(err)
  })
})

test_that("`bottom` must be a positive integer", {
  expect_snapshot((expect_error(trace_back(bottom = -1))))
})

test_that("collapsed case in branch formatting", {
  trace <- new_trace(alist(f(), g(), h(), evalq(), evalq()), 0:4)
  expect_snapshot_output(print(trace, simplify = "branch"))
})

test_that("can detect namespace and scope from call", {
  fn <- set_env(function() NULL, empty_env())

  expect_equal(
    call_trace_context(quote(bar()), fn),
    trace_context()
  )
  expect_equal(
    call_trace_context(quote(foo::bar()), fn),
    trace_context("foo", "::")
  )
  expect_equal(
    call_trace_context(quote(foo:::bar()), fn),
    trace_context("foo", ":::")
  )
})

test_that("trailing `FALSE` visibility is handled", {
  trace <- new_trace(
    alist(f(), g(), h(), foo(), bar()),
    parent = 0:4,
    visible = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_snapshot_trace(trace)
})

test_that("can create empty trace with trace_back()", {
  expect_equal(
    trace_back(top = environment()),
    new_trace(list(), int())
  )
})

test_that("can format empty traces", {
  trace <- new_trace(list(), int())
  expect_snapshot_trace(trace)
})
