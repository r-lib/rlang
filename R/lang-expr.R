#' Evaluate an expression in an environment.
#'
#' \code{expr_eval()} is a lightweight version of the base function
#' \code{\link[base]{eval}()}. It does not accept supplementary data,
#' but it is more efficient and does not clutter the evaluation stack.
#' Technically, \code{expr_eval()} is a simple wrapper around the C
#' function \code{Rf_eval()}.
#'
#' \code{base::eval()} inserts two call frames in the stack, the
#' second of which features the \code{envir} parameter as frame
#' environment. This may unnecessarily clutter the evaluation stack
#' and it can change evaluation semantics with stack sensitive
#' functions in the case where \code{env} is an evaluation environment
#' of a stack frame (see \code{\link{eval_stack}()}). Since the base
#' function \code{eval()} creates a new evaluation context with
#' \code{env} as frame environment there are actually two contexts
#' with the same evaluation environment on the stack when \code{expr}
#' is evaluated. Thus, any command that looks up frames on the stack
#' (stack sensitive functions) may find the parasite frame set up by
#' \code{eval()} rather than the original frame targetted by
#' \code{env}. As a result, code evaluated with \code{base::eval()}
#' does not have the property of stack consistency, and stack
#' sensitive functions like \code{\link[base]{return}()},
#' \code{\link[base]{parent.frame}()} may return misleading results.
#'
#' @param expr An expression to evaluate.
#' @param env The environment in which to evaluate the expression.
#' @useDynLib rlang rlang_eval
#' @seealso with_env
#' @export
#' @examples
#' # expr_eval() works just like base::eval():
#' env <- new_env(data = list(foo = "bar"))
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
      dot_call <- new_call(expr[[1]], quote(...))
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
