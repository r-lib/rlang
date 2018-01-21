#' Extract dots with splicing semantics
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
#'
#' @section Life cycle:
#'
#' * `dots_splice()` is experimental. It is part of our experiments
#'   with dots semantics. We now lean towards adopting a single type
#'   of dots semantics (those of `dots_list()`) where splicing is
#'   explicit.
#'
#' @param ... Arguments with explicit (`dots_list()`) or list
#'   (`dots_splice()`) splicing semantics. The contents of spliced
#'   arguments are embedded in the returned list.
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty.
#' @return A list of arguments. This list is always named: unnamed
#'   arguments are named with the empty string `""`.
#' @seealso [exprs()] for extracting dots without evaluation.
#' @export
#' @examples
#' # Compared to simply using list(...) to capture dots, dots_list()
#' # splices explicitly:
#' x <- list(1, 2)
#' dots_list(!!! x, 3)
#'
#' # Unlike dots_splice(), it doesn't splice bare lists:
#' dots_list(x, 3)
#'
#' # Splicing is also helpful to workaround exact and partial matching
#' # of arguments. Let's create a function taking named arguments and
#' # dots:
#' fn <- function(data, ...) {
#'   dots_list(...)
#' }
#'
#' # You normally cannot pass an argument named `data` through the dots
#' # as it will match `fn`'s `data` argument. The splicing syntax
#' # provides a workaround:
#' fn(some_data, !!! list(data = letters))
dots_list <- function(...,
                      .ignore_empty = c("trailing", "none", "all")) {
  dots <- .Call(rlang_dots_list, environment(), FALSE, .ignore_empty, TRUE)
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
#' dots_splice(!!! x, 3)
#' dots_splice(x, 3)
dots_splice <- function(...,
                        .ignore_empty = c("trailing", "none", "all")) {
  dots <- .Call(rlang_dots_flat_list, environment(), FALSE, .ignore_empty, TRUE)
  names(dots) <- names2(dots)
  dots
}

#' Evaluate dots with preliminary splicing
#'
#' This is a tool for advanced users. It captures dots, processes
#' unquoting and splicing operators, and evaluates them. Unlike
#' [dots_list()] and [dots_splice()], it does not flatten spliced
#' objects. They are merely attributed a `spliced` class (see
#' [splice()]). You can process spliced objects manually, perhaps with
#' a custom predicate (see [flatten_if()]).
#'
#' @inheritParams dots_list
#' @param ... Arguments to evaluate and process splicing operators.
#' @export
#' @examples
#' dots <- dots_values(!!! list(1))
#' dots
#'
#' # Flatten the spliced objects:
#' flatten_if(dots, is_spliced)
dots_values <- function(...,
                        .ignore_empty = c("trailing", "none", "all")) {
  .Call(rlang_dots_values, environment(), FALSE, .ignore_empty, TRUE)
}

#' Capture definition objects
#'
#' @section Life cycle:
#'
#' `dots_definitions()` is experimental. Expect API changes.
#'
#' @inheritParams quotation
#'
#' @keywords internal
#' @export
dots_definitions <- function(...,
                             .named = FALSE,
                             .ignore_empty = c("trailing", "none", "all")) {
  dots <- .Call(rlang_quos_interp, environment(), .named, .ignore_empty, FALSE)

  is_def <- map_lgl(dots, function(dot) is_definition(quo_get_expr(dot)))
  defs <- map(dots[is_def], as_definition)

  list(dots = dots[!is_def], defs = defs)
}
as_definition <- function(def) {
  # The definition comes wrapped in a quosure
  env <- quo_get_env(def)
  def <- quo_get_expr(def)

  list(
    lhs = new_quosure(f_lhs(def), env),
    rhs = new_quosure(f_rhs(def), env)
  )
}

dots_node <- function(...) {
  node_cdr(sys.call())
}

#' How many arguments are currently forwarded in dots?
#'
#' This returns the number of arguments currently forwarded in `...`
#' as an integer.
#'
#' @param ... Forwarded arguments.
#' @export
#' @examples
#' fn <- function(...) dots_n(..., baz)
#' fn(foo, bar)
dots_n <- function(...) {
  nargs()
}
