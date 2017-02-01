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

#' Evaluate an expression in an environment.
#'
#' \code{expr_eval()} is a lightweight version of the base function
#' \code{\link[base]{eval}()}. It does not accept supplementary data,
#' but it is more efficient and does not clutter the evaluation stack.
#' Technically, \code{expr_eval()} is a simple wrapper around the C
#' function \code{Rf_eval()}.
#'
#' \code{base::eval()} inserts two call frames in the stack, the
#' second of which has the supplied environment as frame
#' environment. This may unnecessarily clutter the evaluation stack
#' and it can change base R semantics when evaluating stack sensitive
#' functions like \code{return()} or \code{parent.frame()}. This is
#' because these functions search through the stack for the first
#' occurrence of the environment in which they are evaluated (it is
#' not necessarily the parent frame because of lazy evaluation
#' semantics, see \code{\link{eval_stack}()} and
#' \code{\link{call_stack}()} for more on that distinction). One
#' consequence is that code evaluated with \code{base::eval()} does
#' not have the property of stack consistency, and stack sensitive
#' functions may return misleading results.
#'
#' @param expr An expression to evaluate.
#' @param env The environment in which to evaluate the expression.
#' @useDynLib rlang rlang_eval
#' @seealso with_env
#' @export
#' @examples
#' # expr_eval() works just like base::eval():
#' env <- env_new(dict = list(foo = "bar"))
#' expr <- quote(foo)
#' expr_eval(expr, env)
#'
#' # To explore the consequences of stack inconsistent semantics, let's
#' # create a function that evaluates `parent.frame()` deep in the call
#' # stack, in an environment corresponding to a frame in the middle of
#' # the stack. For consistency we R's lazy evaluation semantics, we'd
#' # expect to get the caller of that frame as result:
#' fn <- function(eval_fn) {
#'   list(
#'     returned_env = middle(eval_fn),
#'     actual_env = env()
#'   )
#' }
#' middle <- function(eval_fn) {
#'   deep(eval_fn, env())
#' }
#' deep <- function(eval_fn, eval_env) {
#'   expr <- quote(parent.frame())
#'   eval_fn(expr, eval_env)
#' }
#'
#' # With expr_eval(), we do get the expected environment:
#' fn(rlang::expr_eval)
#'
#' # But that's not the case with base::eval():
#' fn(base::eval)
#'
#' # Another difference of expr_eval() compared to base::eval() is
#' # that it does not insert parasite frames in the evaluation stack:
#' get_stack <- quote(identity(eval_stack()))
#' expr_eval(get_stack)
#' eval(get_stack)
expr_eval <- function(expr, env = parent.frame()) {
  .Call(rlang_eval, expr, env)
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
