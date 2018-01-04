#' Tidy quotation of multiple expressions and dots
#'
#' @description
#'
#' * `quos()` quotes its arguments and returns them as a list of
#'   quosures (see [quo()]).
#'
#' * `enquos()` should be used inside a function. It takes argument
#'   names and captures the expressions to those arguments, i.e. one
#'   level up.
#'
#'
#' @section The := operator:
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
#'
#' @param ... For `enquos()`, names of arguments to capture without
#'   evaluation (including `...`). For `quos()`, the expressions to
#'   capture unevaluated (including expressions contained in `...`).
#' @param .named Whether to ensure all dots are named. Unnamed
#'   elements are processed with [expr_text()] to figure out a default
#'   name. If an integer, it is passed to the `width` argument of
#'   `expr_text()`, if `TRUE`, the default width is used. See
#'   [exprs_auto_name()].
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty.
#' @param .unquote_names Whether to treat `:=` as `=`. Unlike `=`, the
#'   `:=` syntax supports `!!` unquoting on the LHS.
#' @export
#' @name quosures
#' @examples
#' # quos() is like the singular version quo() but allows quoting
#' # several arguments:
#' quos(foo(), letters[1:2], !! letters[1:2])
#'
#' # enquos() is like enquo() but lets you capture several arguments:
#' fn <- function(arg1, arg2, ...) {
#'   enquos(arg1, arg2, ...)
#' }
#' fn(leonardo, donatello, raphael())
#'
#' # Because of the nature of dots forwarding (expressions are
#' # "teleported"), quos() can also be used to capture dots. But
#' # notice the difference with enquos(). Arguments other than `...`
#' # are captured directly, within the function:
#' fn <- function(arg1, arg2, ...) {
#'   quos(arg1, arg2, ...)
#' }
#' fn(leonardo, donatello, raphael())
#'
#'
#' # Lists of arguments can be spliced in:
#' args <- list(x = 1:3, y = ~var)
#' quos(!!! args, z = 10L)
#'
#' # As well as lists of bare expressions (make sure that the current
#' # environment is where the symbols in these expressions are defined):
#' args <- alist(mouse1 = bernard, mouse2 = bianca)
#' quos(!!! args)
#'
#'
#' # Like `=`, the `:=` operator creates named arguments:
#' quos(mouse1 := bernard, mouse2 = bianca)
#'
#' # The `:=` is mainly useful to unquote names. Unlike `=` it
#' # supports `!!` on its LHS:
#' var <- "unquote me!"
#' quos(!!var := bernard, mouse2 = bianca)
#'
#'
#' # All these features apply to dots captured by enquos():
#' fn <- function(...) enquos(...)
#' fn(
#'   !!! args,
#'   !!var := penny
#' )
quos <- function(...,
                 .named = FALSE,
                 .ignore_empty = c("trailing", "none", "all"),
                 .unquote_names = TRUE) {
  .Call(rlang_quos_interp, environment(), .named, .ignore_empty, .unquote_names)
}

#' @rdname quosures
#' @export
enquos <- function(...) {
  syms <- as.list(node_cdr(sys.call()))
  env <- parent.frame()

  splice_dots <- FALSE
  quos <- map(syms, function(sym) {
    if (!is_symbol(sym)) {
      abort("Inputs to capture must be argument names")
    }
    if (identical(sym, dots_sym)) {
      splice_dots <<- TRUE
      splice(.Call(rlang_quos_interp, env, FALSE, "none", TRUE))
    } else {
      .Call(rlang_enquo, sym, env)
    }
  })

  if (splice_dots) {
    quos <- flatten_if(quos, is_spliced)
  }
  names(quos) <- names2(quos)
  structure(quos, class = "quosures")
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
#' @export
print.quosures <- function(x, ...) {
  print(unclass(x), ...)
}

#' Ensure that list of expressions are all named
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or
#' quosures). `exprs_auto_name()` deparses the expressions with
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
