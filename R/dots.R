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
#' These quoting operators take dots and expect two-sided patterns
#' like `a ~ b` or `a := b`. The LHS and RHS of each input are
#' extracted in a list of quosures. The operators differ in the type
#' of inputs they accept:
#'
#' * `dots_patterns()` accepts both two-sided formulas and `:=`
#'   definitions. There is no distinction between either type of
#'   inputs. This lets your users choose which syntax they prefer.
#'
#' * `dots_formulas()` and `dots_definitions()` accept only one type
#'   of inputs.
#'
#' @inheritParams quosures
#' @return A named list of list of extracted LHS and RHS
#'   components. The components are extracted as quosures in a list
#'   with `lhs` and `rhs` fields. The outer list is thus named after
#'   the dots inputs while the inner lists contain the components for
#'   each input.
#' @seealso [quos()]
#' @name dots_patterns
#' @examples
#' dots_formulas(
#'   a ~ b,
#'   c ~ d
#' )
#'
#' dots_definitions(
#'   a := b,
#'   c := d
#' )
#'
#' # dots_patterns() supports both kinds of inputs at the same time
#' # but this should be discouraged. Users should opt for one type and
#' # stick to it consistently:
#' dots_patterns(
#'   a ~ b,
#'   c := d
#' )
NULL

dots_patterns_ctor <- function(check_dots) {
  fn <- function(...,
                 .named = FALSE,
                 .ignore_empty = c("trailing", "none", "all")) {
    NULL
  }
  environment(fn) <- parent.frame()

  body(fn) <- bquote({
    dots <- .Call(rlang_quos_interp, environment(), .named, .ignore_empty, FALSE)

    .(check_dots)(dots)

    map(dots, pattern_quos)
  })

  fn
}

#' @rdname dots_patterns
#' @export
dots_patterns <- dots_patterns_ctor(quote(check_patterns))
#' @rdname dots_patterns
#' @export
dots_formulas <- dots_patterns_ctor(quote(check_formulas))
#' @rdname dots_patterns
#' @export
dots_definitions <- dots_patterns_ctor(quote(check_definitions))

check_patterns <- function(dots) {
  if (!every(dots, quo_is_formulaish, lhs = TRUE)) {
    abort("`...` can only contain two-sided formulas or `:=` definitions")
  }
}
check_formulas <- function(dots) {
  if (!every(dots, quo_is_formula)) {
    abort("`...` can only contain two-sided formulas")
  }
}
check_definitions <- function(dots) {
  if (!every(dots, quo_is_definition)) {
    abort("`...` can only contain `:=` definitions")
  }
}

pattern_quos <- function(x, default_env = NULL) {
  # The pattern may be wrapped in a quosure
  if (is_quosure(x)) {
    env <- get_env(x)
    x <- get_expr(x)
  } else {
    env <- default_env
  }
  stopifnot(is_formulaish(x, lhs = TRUE))
  stopifnot(is_env(env))

  list(
    lhs = new_quosure(f_lhs(x), env),
    rhs = new_quosure(f_rhs(x), env)
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
