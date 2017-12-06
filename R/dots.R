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
#' @inheritParams quosures
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
                      .ignore_empty = c("trailing", "none", "all"),
                      .unquote_names = TRUE) {
  dots <- .Call(rlang_dots_list, environment(), FALSE, .ignore_empty, .unquote_names)
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
                        .ignore_empty = c("trailing", "none", "all"),
                        .unquote_names = TRUE) {
  dots <- .Call(rlang_dots_flat_list, environment(), FALSE, .ignore_empty, .unquote_names)
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
#' @inheritParams quosures
#' @param ... Arguments to evaluate and process splicing operators.
#' @export
#' @examples
#' dots <- dots_values(!!! list(1))
#' dots
#'
#' # Flatten the spliced objects:
#' flatten_if(dots, is_spliced)
dots_values <- function(...,
                        .ignore_empty = c("trailing", "none", "all"),
                        .unquote_names = TRUE) {
  .Call(rlang_dots_values, environment(), FALSE, .ignore_empty, .unquote_names)
}

#' Quote patterns, definitions or formulas
#'
#' @description
#'
#' These quoting functions take dots and expect two-sided patterns
#' such as `a ~ b` or `a := b`. The LHS and RHS of each input are
#' extracted in a list containing two quosures.
#'
#' * `dots_patterns()` expects two-sided formulas by default. If
#'   `.operator` is set to `":="`, it expects definitions.
#'
#' * `dots_definitions()` extracts `:=` definitions in a sublist named
#'   `defs`. All other types of inputs are captured in quosures within
#'   `dots`.
#'
#' @inheritParams quosures
#' @param .operator The name of a binary operator from which to
#'   extract an LHS and a RHS.
#' @return A named list of lists of extracted LHS and RHS
#'   components. The inner lists contain two quosures named `lhs` and
#'   `rhs`. The outer list is named after the dots inputs.
#' @seealso [quos()]
#' @export
#' @examples
#' dots_patterns(
#'   A = a ~ b,
#'   c ~ d
#' )
#'
#' # You can specify the binary operator:
#' dots_patterns(a := b, .operator = ":=")
#' dots_patterns(a / b, .operator = "/")
dots_patterns <- function(...,
                          .named = FALSE,
                          .ignore_empty = c("trailing", "none", "all"),
                          .operator = "~") {
  dots <- .Call(rlang_quos_interp, environment(), .named, .ignore_empty, FALSE)

  stopifnot(is_string(.operator))
  predicate <- switch(.operator,
    `~` = function(x) quo_is_formula(x, lhs = TRUE),
    `:=` = quo_is_definition,
    function(x) is_lang(quo_get_expr(x), .operator, n = 2)
  )
  if (!every(dots, predicate)) {
    type_name <- switch(.operator,
      `~` = "two-sided formulas",
      `:=` = "`:=` definitions",
      paste0("calls to `", .operator, "` with two arguments")
    )
    abort(paste0("`...` can only contain ", type_name))
  }

  map(dots, pattern_quos)
}
#' @rdname dots_patterns
#' @export
dots_definitions <- function(...,
                             .named = FALSE,
                             .ignore_empty = c("trailing", "none", "all")) {
  dots <- .Call(rlang_quos_interp, environment(), .named, .ignore_empty, FALSE)

  is_def <- map_lgl(dots, quo_is_definition)
  defs <- map(dots[is_def], pattern_quos)

  list(dots = dots[!is_def], defs = defs)
}
pattern_quos <- function(x, default_env = NULL) {
  # The pattern may be wrapped in a quosure
  if (is_quosure(x)) {
    env <- quo_get_env(x)
    x <- quo_get_expr(x)
  } else {
    env <- default_env
  }
  stopifnot(is_lang(x, n = 2))
  stopifnot(is_env(env))

  list(
    lhs = new_quosure(node_cadr(x), env),
    rhs = new_quosure(node_cadr(node_cdr(x)), env)
  )
}

is_dot_symbol <- function(x) {
  is_symbol(x) && is_dot_nm(as.character(x))
}
is_dot_nm <- function(nm) {
  grepl("^\\.\\.[0-9]+$", nm)
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
