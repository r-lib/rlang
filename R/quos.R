#' Tidy quotation of multiple expressions and dots
#'
#' `quos()` quotes its arguments and returns them as a list of
#' quosures (see [quo()]). It is especially useful to capture
#' arguments forwarded through `...`.
#'
#' Both `quos` and `dots_definitions()` have specific support for
#' definition expressions of the type `var := expr`, with some
#' differences:
#'
#'\describe{
#'  \item{`quos()`}{
#'    When `:=` definitions are supplied to `quos()`, they are treated
#'    as a synonym of argument assignment `=`. On the other hand, they
#'    allow unquoting operators on the left-hand side, which makes it
#'    easy to assign names programmatically.}
#'  \item{`dots_definitions()`}{
#'    This dots capturing function returns definitions as is. Unquote
#'    operators are processed on capture, in both the LHS and the
#'    RHS. Unlike `quos()`, it allows named definitions.}
#' }
#' @param ... Expressions to capture unevaluated.
#' @inheritParams dots_values
#' @param .named Whether to ensure all dots are named. Unnamed
#'   elements are processed with [expr_text()] to figure out a default
#'   name. If an integer, it is passed to the `width` argument of
#'   `expr_text()`, if `TRUE`, the default width is used. See
#'   [exprs_auto_name()].
#' @export
#' @name quosures
#' @examples
#' # quos() is like the singular version but allows quoting
#' # several arguments:
#' quos(foo(), bar(baz), letters[1:2], !! letters[1:2])
#'
#' # It is most useful when used with dots. This allows quoting
#' # expressions across different levels of function calls:
#' fn <- function(...) quos(...)
#' fn(foo(bar), baz)
#'
#' # Note that quos() does not check for duplicate named
#' # arguments:
#' fn <- function(...) quos(x = x, ...)
#' fn(x = a + b)
#'
#'
#' # Dots can be spliced in:
#' args <- list(x = 1:3, y = ~var)
#' quos(!!! args, z = 10L)
#'
#' # Raw expressions are turned to formulas:
#' args <- alist(x = foo, y = bar)
#' quos(!!! args)
#'
#'
#' # Definitions are treated similarly to named arguments:
#' quos(x := expr, y = expr)
#'
#' # However, the LHS of definitions can be unquoted. The return value
#' # must be a symbol or a string:
#' var <- "foo"
#' quos(!!var := expr)
#'
#' # If you need the full LHS expression, use dots_definitions():
#' dots <- dots_definitions(var = foo(baz) := bar(baz))
#' dots$defs
quos <- function(..., .named = FALSE,
                 .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_enquose(...)
  dots <- dots_clean_empty(dots, quo_is_missing, .ignore_empty)

  if (.named) {
    width <- quo_names_width(.named)
    dots <- quos_auto_name(dots, width)
  }
  set_attrs(dots, class = "quosures")
}

#' @rdname quosures
#' @param x An object to test.
#' @export
is_quosures <- function(x) {
  inherits(x, "quosures")
}
#' @export
`[.quosures` <- function(x, i) {
  set_attrs(NextMethod(), class = "quosures")
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

#' Ensure that list of expressions are all named
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or tidy
#' quotes). `exprs_auto_name()` deparses the expressions with
#' [expr_text()] by default. `quos_auto_name()` deparses with
#' [quo_text()].
#'
#' @param exprs A list of expressions.
#' @param width Maximum width of names.
#' @param printer A function that takes an expression and converts it
#'   to a string. This function must take an expression as first
#'   argument and `width` as second argument.
#' @export
exprs_auto_name <- function(exprs, width = 60L, printer = expr_text) {
  have_name <- have_name(exprs)

  if (any(!have_name)) {
    nms <- map_chr(exprs[!have_name], printer, width = width)
    names(exprs)[!have_name] <- nms
  }

  exprs
}
#' @rdname exprs_auto_name
#' @param quos A list of quosures.
#' @export
quos_auto_name <- function(quos, width = 60L) {
  exprs_auto_name(quos, width = width, printer = quo_text)
}
