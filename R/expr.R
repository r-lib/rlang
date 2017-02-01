#' Untidy quotation of an expression.
#'
#' Unlike \code{\link{tidy_quote}()}, \code{expr_quote()} returns a
#' raw expression instead of a formula. As a result,
#' \code{expr_quote()} is untidy in the sense that it does not
#' preserve scope information for the quoted expression. It can still
#' be useful in certain cases. Compared to base R's
#' \code{\link[base]{quote}()}, it unquotes the expression on capture,
#' and compared to \code{\link{tidy_quote}()}, the quoted expression
#' is directly compatible with the base R \code{\link[base]{eval}()}
#' function.
#'
#' @inheritParams tidy_quote
#' @seealso See \code{\link{tidy_quote}()} and
#'   \code{\link{tidy_interp}()} for more explanation on tidy
#'   quotation.
#' @return The raw expression supplied as argument.
#' @export
#' @examples
#' # The advantage of expr_quote() over quote() is that it unquotes on
#' # capture:
#' expr_quote(list(1, !! 3 + 10))
#'
#' # Unquoting can be especially useful for successive transformation
#' # of a captured expression:
#' (expr <- quote(foo(bar)))
#' (expr <- expr_quote(inner(!! expr, arg1)))
#' (expr <- expr_quote(outer(!! expr, !!! lapply(letters[1:3], as.symbol))))
#'
#' # Unlike tidy_quote(), expr_quote() produces expressions that can
#' # be evaluated with base::eval():
#' e <- quote(letters)
#' e <- expr_quote(toupper(!!e))
#' eval(e)
#'
#' # Be careful if you unquote a formula-quote: you need to take the
#' # RHS (and lose the scope information) to evaluate with eval():
#' f <- ~letters
#' e <- expr_quote(toupper(!! f_rhs(f)))
#' eval(e)
#'
#' # However it's fine to unquote formulas if you evaluate with tidy_eval():
#' f <- ~letters
#' e <- expr_quote(toupper(!! f))
#' tidy_eval(e)
expr_quote <- function(expr) {
  expr <- substitute(expr)
  .Call(interp_, expr, parent.frame(), FALSE)
}


#' Turn an expression to a label.
#'
#' \code{expr_text()} turns the expression into a single string;
#' \code{expr_label()} formats it nicely for use in messages.
#'
#' @param expr An expression to labellise.
#' @export
#' @examples
#' # To labellise a function argument, first capture it with
#' # substitute():
#' fn <- function(x) expr_label(substitute(x))
#' fn(x:y)
#'
#' # Strings are encoded
#' expr_label("a\nb")
#'
#' # Names and expressions are quoted with ``
#' expr_label(quote(x))
#' expr_label(quote(a + b + c))
#'
#' # Long expressions are collapsed
#' expr_label(quote(foo({
#'   1 + 2
#'   print(x)
#' })))
expr_label <- function(expr) {
  if (is.character(expr)) {
    encodeString(expr, quote = '"')
  } else if (is.atomic(expr)) {
    format(expr)
  } else if (is.name(expr)) {
    paste0("`", as.character(expr), "`")
  } else {
    chr <- deparse(expr)
    if (length(chr) > 1) {
      dot_call <- call_new(expr[[1]], quote(...))
      chr <- paste(deparse(dot_call), collapse = "\n")
    }
    paste0("`", chr, "`")
  }
}

#' @export
#' @rdname expr_label
#' @param width Width of each line.
#' @param nlines Maximum number of lines to extract.
expr_text <- function(expr, width = 60L, nlines = Inf) {
  str <- deparse(expr, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}
