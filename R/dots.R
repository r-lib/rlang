#' Extract dots with splicing semantics.
#'
#' These functions evaluate all arguments contained in `...` and
#' return them as a list. They both splice their arguments if they
#' qualify for splicing. See [ll()] for information about splicing
#' and below for the kind of arguments that qualify for splicing.
#'
#' `dots_list()` has _explicit splicing semantics_: it splices lists
#' that are explicitly marked for [splicing][ll] with the
#' [splice()] adjective. `dots_splice()` on the other hand has _list
#' splicing semantics_: in addition to lists marked explicitly for
#' splicing, [bare][is_bare_list] lists are spliced as well.
#'
#' @inheritParams dots_values
#' @param ... Arguments with explicit (`dots_list()`) or list
#'   (`dots_splice()`) splicing semantics. The contents of spliced
#'   arguments are embedded in the returned list.
#' @return A list of arguments. This list is always named: unnamed
#'   arguments are named with the empty string `""`.
#' @seealso [exprs()] for extracting dots without evaluation.
#' @export
#' @examples
#' # Compared to simply using list(...) to capture dots, dots_list()
#' # splices explicitly:
#' x <- list(1, 2)
#' dots_list(splice(x), 3)
#'
#' # Unlike dots_splice(), it doesn't splice bare lists:
#' dots_list(x, 3)
dots_list <- function(..., .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_values(..., .ignore_empty = .ignore_empty)
  dots <- .Call(rlang_squash, dots, "list", is_spliced, 1L)
  names(dots) <- names2(dots)
  dots
}
#' @rdname dots_list
#' @export
#' @examples
#'
#' # dots_splice() splices lists marked with splice() as well as bare
#' # lists:
#' x <- list(1, 2)
#' dots_splice(splice(x), 3)
#' dots_splice(x, 3)
dots_splice <- function(..., .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_values(..., .ignore_empty = .ignore_empty)
  dots <- .Call(rlang_squash, dots, "list", is_spliced_bare, 1L)
  names(dots) <- names2(dots)
  dots
}

#' Evaluate dots with preliminary splicing.
#'
#' This is a tool for advanced users. It captures dots, processes
#' unquoting and splicing operators, and evaluates them. Unlike
#' [dots_list()] and [dots_splice()], it does not flatten spliced
#' objects. They are merely attributed a `spliced` class (see
#' [splice()]). You can process spliced objects manually, perhaps with
#' a custom predicate (see [flatten_if()]).
#'
#' @param ... Arguments to evaluate and process splicing operators.
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty.
#' @export
#' @examples
#' dots <- dots_values(!!! list(1))
#' dots
#'
#' # Flatten the spliced objects:
#' flatten_if(dots, is_spliced)
dots_values <- function(..., .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_capture(..., `__quosured` = FALSE)
  dots <- dots_clean_empty(dots, function(x) is_missing(x$expr), .ignore_empty)
  if (is_null(dots)) {
    dots <- list(...)
    set_names(dots, names2(dots))
  } else {
    map(dots, function(dot) eval_bare(dot$expr, dot$env))
  }
}

dots_clean_empty <- function(dots, is_empty, ignore_empty) {
  n_dots <- length(dots)

  if (n_dots) {
    which <- arg_match(ignore_empty, c("trailing", "none", "all"))
    switch(which,
      trailing =
        if (is_empty(dots[[n_dots]])) {
          dots[[n_dots]] <- NULL
        },
      all = {
        dots <- discard(dots, is_empty)
      }
    )
  }

  dots
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
#' [exprs()], and a call stack as returned by
#' [call_stack()].
#'
#' @param ... Dots to inspect.
#' @param .only_dots,only_dots Whether to stop introspection once
#'   forwarded dots have been climbed. Setting this to `TRUE` is only
#'   useful for inspecting dots (cf. [quos()][quosures] which does not
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
  dots <- exprs(...)
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
    dot_name <- sym(paste0("..", i))
    mut_node_car(dot, dot_name)
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
