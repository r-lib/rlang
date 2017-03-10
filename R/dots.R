#' Extract dots with explicit splicing.
#'
#' `dots_list()` evaluates all arguments contained in `...` and returns
#' them as a list. It splices lists that are explicitly marked for
#' [splicing][splice] with the [spliced()] adjective. See
#' [dots_splice()] for a version that splices bare lists as well.
#'
#' `dots_list()` and `dots_splice()` are simple aliases to
#' [splice()]. Their main purpose is to provide documentation for the
#' `...` argument that developers can inherit from in their packages.
#'
#' @param ... Arguments with explicit splicing semantics. Arguments
#'   marked with [rlang::spliced()] are spliced in the returned list.
#' @seealso [dots_splice()] for full splicing semantics, and
#'   [dots_exprs()] for extracting dots without evaluation.
#' @export
#' @examples
#' # Compared to simply using list(...) to capture dots, dots_list()
#' # splices explicitly:
#' x <- list(1, 2)
#' dots_list(spliced(x), 3)
#'
#' # Unlike dots_splice(), it doesn't splice bare lists:
#' dots_list(x, 3)
dots_list <- function(...) {
  splice(..., .bare = FALSE)
}

#' Extract dots with full splicing.
#'
#' Like [dots_list()], `dots_splice()` evaluates all arguments
#' contained in `...` and returns them as a list. It has full splicing
#' semantics: in addition to lists marked explicitly for splicing,
#' [bare lists][is_bare_list] are spliced as well. See [dots_list()]
#' for a version that only splices explicitly.
#'
#' `dots_list()` and `dots_splice()` are simple aliases to
#' [splice()]. Their main purpose is to provide documentation for the
#' `...` argument that developers can inherit from in their packages.
#'
#' @param ... Arguments with full splicing semantics. [Bare
#'   lists][is_bare_list] and arguments marked with [rlang::spliced()]
#'   are spliced in the returned list.
#' @seealso [dots_list()] for explicit splicing semantics, and
#'   [dots_exprs()] for extracting dots without evaluation.
#' @export
#' @examples
#' # dots_splice() splices lists marked with spliced() as well as bare
#' # lists:
#' x <- list(1, 2)
#' dots_splice(spliced(x), 3)
#' dots_splice(x, 3)
dots_splice <- function(...) {
  splice(..., .bare = TRUE)
}

#' Extract dots forwarded as arguments.
#'
#' These functions return the arguments forwarded through `...`.
#' Contrarily to [dots_list()] and [dots_splice()], `dots_exprs()` and
#' `dots_node()` do not evaluate the arguments. The former returns a
#' list of expressions while the latter returns a [pairlist].
#'
#' @param ... Arguments to extract.
#' @seealso [frame_dots_exprs()]
#' @export
dots_exprs <- function(...) {
  expr_eval(substitute(alist(...)))
}
#' @rdname dots_exprs
#' @export
dots_node <- function(...) {
  dots <- node_cdr(substitute(alist(...)))
  if (is_symbolic(dots)) {
    NULL
  } else {
    dots
  }
}

#' Extract dots from a frame.
#'
#' These functions extract dots from a frame environment on the
#' [evaluation stack][eval_stack]. The `_node()` version return a
#' pairlist that is ready to be spliced into a call, while the regular
#' version return a regular list that is usually easier to work with.
#'
#' `frame_dots_exprs()` and `frame_dots_node()` never fail, even if the
#' frame does not contain dots. Instead they return an empty list or
#' `NULL` respectively.
#'
#' @param frame The environment from which the dots should be
#'   retrieved. Can be a frame, an environment, or a formula from
#'   which to retrieve an environment. If not supplied, the calling
#'   frame is used.
#' @param ... Arguments to extract. Can be both forwarded dots and
#'   direct arguments.
#' @export
frame_dots_exprs <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  as.list(frame_dots_node(frame))
}

#' @rdname frame_dots_exprs
#' @export
frame_dots_node <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  env <- get_env(frame)

  dots <- node_cdr(substitute(alist(...), env))
  if (is_symbolic(dots)) {
    NULL
  } else {
    dots
  }
}

#' Inspect dots
#'
#' Runs [arg_inspect()] for each dots element, and return the results
#' in a list.
#'
#' `only_dots` controls whether dotted arguments should be fully or
#' partially inspected. When `TRUE`, only forwarded dots are
#' climbed. Symbols bound to a promise are not. See the example
#' section.
#'
#' `dots_inspect_()` is the standard evaluation version of
#' `dots_inspect()` and takes a list of dots as captured by
#' [frame_dots_exprs()] or [dots_exprs()], and a call stack as returned by
#' [call_stack()].
#'
#' @param ... Dots to inspect.
#' @param .only_dots,only_dots Whether to stop introspection once
#'   forwarded dots have been climbed. Setting this to `TRUE` is only
#'   useful for inspecting dots (cf. [tidy_quotes()] which does not
#'   follow symbols).
#' @seealso [arg_inspect()]
#' @export
#' @examples
#' # The following example focuses on the difference between full and
#' # partial introspection of dots, which can be difficult to grasp.
#'
#' h <- function(...) {
#'   # Let's parameterise `only_dots` with a global variable
#'   dots_inspect(..., .only_dots = only_dots)
#' }
#'
#' g <- function(...) {
#'   # Here dots are forwarded from g to h. The first node of `...` in
#'   # h's frame environment is bound to a promise, pledging to evaluate
#'   # `..1` in g's frame environment.
#'   h(...)
#' }
#'
#' f <- function(arg) {
#'   # Here the first node of `...` in g's frame environment is bound to
#'   # a promise, pledging to evaluate `arg` in f's frame environment.
#'   g(arg)
#' }
#'
#' # Here `arg` is bound to a promise, pledging to evaluate `foo(bar)`
#' # in the global environment. We request full
#' # introspection. Arguments are climbed beyond forwarded dots and
#' # introspection is given the same scope as lazy
#' # evaluation. dots_inspect() thus returns information about `arg`
#' only_dots <- FALSE
#' f(foo(bar))
#'
#' # Here, while `arg` is bound to a promise just like in the last
#' # call, argument instrospection is not given the same scope as lazy
#' # evaluation. dots_inspect() does not follow symbols bound to a
#' # promise and thus returns information about ..1, the expression
#' # supplied at the first call site (before forwarding dots). The
#' # expression is `arg` (a symbol that is bound to a promise), to be
#' # evaluated in f's call frame.
#' only_dots <- TRUE
#' f(foo(bar))
dots_inspect <- function(..., .only_dots = FALSE) {
  dots <- dots_exprs(...)
  stack <- call_stack()
  dots_inspect_(dots, stack, only_dots = .only_dots)
}

#' @rdname dots_inspect
#' @inheritParams arg_inspect_
#' @param dots Dots to inspect.
#' @export
dots_inspect_ <- function(dots, stack, only_dots = FALSE) {
  dots_syms <- dots_enumerate_sym(dots)
  dots_syms <- set_names(dots_syms, names(dots))
  map(dots_syms, arg_inspect_, stack, only_dots = only_dots)
}

dots_enumerate <- function(dots) {
  if (!length(dots)) {
    character(0)
  } else {
    paste0("..", seq_along(dots))
  }
}
dots_enumerate_sym <- function(dots) {
  nms <- dots_enumerate(dots)
  if (!length(nms)) {
    dots
  } else {
    map(nms, as.name)
  }
}
dots_enumerate_args <- function(dots) {
  i <- 1
  node_walk(dots, function(dot) {
    dot_name <- as.symbol(paste0("..", i))
    set_node_car(dot, dot_name)
    i <<- i + 1
  })
  dots
}

#' Is object a ..n symbol?
#' @param x An object to test.
#' @export
#' @examples
#' is_dot_symbol(quote(..2))
#' is_dot_symbol(quote(sym))
is_dot_symbol <- function(x) {
  is_symbol(x) && is_dot_nm(as.character(x))
}

is_dot_nm <- function(nm) {
  grepl("^\\.\\.[0-9]+$", nm)
}
