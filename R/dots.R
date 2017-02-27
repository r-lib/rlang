#' Extract dots
#'
#' \code{frame_dots()} extracts dots from a frame and
#' \code{dots()} extracts dots from its arguments. The
#' \code{_lsp()} versions return a pairlist that is ready to be
#' spliced into a call, while the regular versions return a regular
#' list that is usually easier to work with.
#'
#' \code{frame_dots()} and \code{frame_dots_lsp()} never fail, even if
#' the frame does not contain dots. Instead they return an empty list
#' or \code{NULL} respectively.
#'
#' @param frame The environment from which the dots should be
#'   retrieved. Can be a frame, an environment, or a formula from
#'   which to retrieve an environment. If not supplied, the calling
#'   frame is used.
#' @param ... Arguments to extract. Can be both forwarded dots and
#'   direct arguments.
#' @export
frame_dots <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  as.list(frame_dots_lsp(frame))
}

#' @rdname frame_dots
#' @export
frame_dots_lsp <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  env <- get_env(frame)

  dots <- cdr(substitute(alist(...), env))
  if (is.language(dots)) {
    NULL
  } else {
    dots
  }
}

#' @rdname frame_dots
#' @export
dots <- function(...) {
  expr_eval(substitute(alist(...)))
}

#' @rdname frame_dots
#' @export
dots_lsp <- function(...) {
  dots <- cdr(substitute(alist(...)))
  if (is.language(dots)) {
    NULL
  } else {
    dots
  }
}

#' Inspect dots
#'
#' Runs \code{\link{arg_inspect}()} for each dots element, and return the
#' results in a list.
#'
#' \code{only_dots} controls whether dotted arguments should be fully
#' or partially inspected. When \code{TRUE}, only forwarded dots are
#' climbed. Symbols bound to a promise are not. See the example
#' section.
#'
#' \code{dots_inspect_()} is the standard evaluation version of
#' \code{dots_inspect()} and takes a list of dots as captured by
#' \code{\link{frame_dots}()} or \code{\link{dots}()}, and a call
#' stack as returned by \code{\link{call_stack}()}.
#'
#' @param ... Dots to inspect.
#' @param .only_dots,only_dots Whether to stop introspection once
#'   forwarded dots have been climbed. Setting this to \code{TRUE} is
#'   only useful for inspecting dots (cf. \code{\link{tidy_quotes}()}
#'   which does not follow symbols).
#' @seealso \code{\link{arg_inspect}()}
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
  dots <- dots(...)
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
  lsp_walk(dots, function(dot) {
    dot_name <- as.symbol(paste0("..", i))
    set_car(dot, dot_name)
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
