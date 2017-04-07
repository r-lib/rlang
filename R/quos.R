#' Tidy quotation of multiple expressions and dots.
#'
#' `dots_quos()` quotes its arguments and returns them as a list of
#' tidy quotes. It is especially useful to "capture" arguments
#' forwarded through `...`.
#'
#' Both `dots_quos` and `dots_definitions()` have specific support for
#' definition expressions of the type `var := expr`, with some
#' differences:
#'
#'\describe{
#'  \item{`dots_quos()`}{
#'    When `:=` definitions are supplied to `dots_quos()`,
#'    they are treated as a synonym of argument assignment
#'    `=`. On the other hand, they allow unquoting operators on
#'    the left-hand side, which makes it easy to assign names
#'    programmatically.}
#'  \item{`dots_definitions()`}{
#'    This dots capturing function returns definitions as is. Unquote
#'    operators are processed on capture, in both the LHS and the
#'    RHS. Unlike `dots_quos()`, it allows named definitions.}
#' }
#' @inheritParams enquo
#' @param .named Whether to ensure all dots are named. Unnamed
#'   elements are processed with [expr_text()] to figure out a default
#'   name. If an integer, it is passed to the `width` argument of
#'   `expr_text()`, if `TRUE`, the default width is used. See
#'   [exprs_auto_name()].
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty.
#' @export
#' @name quosures
#' @examples
#' # dots_quos() is like the singular version but allows quoting
#' # several arguments:
#' dots_quos(foo(), bar(baz), letters[1:2], !! letters[1:2])
#'
#' # It is most useful when used with dots. This allows quoting
#' # expressions across different levels of function calls:
#' fn <- function(...) dots_quos(...)
#' fn(foo(bar), baz)
#'
#' # Note that dots_quos() does not check for duplicate named
#' # arguments:
#' fn <- function(...) dots_quos(x = x, ...)
#' fn(x = a + b)
#'
#'
#' # Dots can be spliced in:
#' args <- list(x = 1:3, y = ~var)
#' dots_quos(!!! args, z = 10L)
#'
#' # Raw expressions are turned to formulas:
#' args <- alist(x = foo, y = bar)
#' dots_quos(!!! args)
#'
#'
#' # Definitions are treated similarly to named arguments:
#' dots_quos(x := expr, y = expr)
#'
#' # However, the LHS of definitions can be unquoted. The return value
#' # must be a symbol or a string:
#' var <- "foo"
#' dots_quos(!!var := expr)
#'
#' # If you need the full LHS expression, use dots_definitions():
#' dots <- dots_definitions(var = foo(baz) := bar(baz))
#' dots$defs
dots_quos <- function(..., .named = FALSE,
                      .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_enquose(...)

  n_dots <- length(dots)
  if (n_dots) {
    dots <- switch(match.arg(.ignore_empty),
      trailing =
        if (quo_is_missing(dots[[n_dots]])) {
          dots[[n_dots]] <- NULL
          dots
        } else {
          dots
        },
      all = discard(dots, quo_is_missing),
      dots
    )
  }

  if (.named) {
    width <- quo_names_width(.named)
    dots <- exprs_auto_name(dots, width)
  }
  struct(dots, class = "quosures")
}
#' @rdname quosures
#' @export
quos <- dots_quos

#' @rdname quosures
#' @export
is_quosures <- function(x) {
  inherits(x, "quosures")
}
#' @export
`[.quosures` <- function(x, i) {
  struct(NextMethod(), class = "quosures")
}
#' @export
c.quosures <- function(..., recursive = FALSE) {
  structure(NextMethod(), class = "quosures")
}

quo_names_width <- function(named) {
  if (is_true(named)) {
    60L
  } else if (is_scalar_integerish(named)) {
    named
  } else {
    abort("`.named` must be a scalar logical or a numeric")
  }
}

#' @rdname quosures
#' @export
dots_definitions <- function(..., .named = FALSE) {
  dots <- dots_enquose(..., `__interp_lhs` = FALSE)
  if (.named) {
    width <- quo_names_width(.named)
    dots <- exprs_auto_name(dots, width)
  }

  is_def <- map_lgl(dots, function(dot) is_definition(dot))
  defs <- map(dots[is_def], as_definition)

  list(dots = dots[!is_def], defs = defs)
}

as_definition <- function(def) {
  env <- f_env(def)
  list(
    lhs = new_quosure(f_lhs(def), env),
    rhs = new_quosure(f_rhs(def), env)
  )
}


#' Ensure that list of expressions are all named.
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or tidy
#' quotes). The expressions are deparsed with [expr_text()].
#'
#' @param exprs A list of expressions or expression wrappers,
#'   e.g. tidy quotes.
#' @param width Maximum width of names.
#' @export
exprs_auto_name <- function(exprs, width = 60L) {
  have_name <- have_name(exprs)

  if (any(!have_name)) {
    nms <- map_chr(exprs[!have_name], quo_text, width = width)
    names(exprs)[!have_name] <- nms
  }

  exprs
}
