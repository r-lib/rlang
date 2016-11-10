#' Create a function by "hand"
#'
#' This constructs a new function given it's three components:
#' list of arguments, body code and parent environment.
#'
#' @param args A named list of default arguments.  Note that if you want
#'  arguments that don't have defaults, you'll need to use the special function
#'  \code{\link{alist}}, e.g. \code{alist(a = , b = 1)}
#' @param body A language object representing the code inside the function.
#'   Usually this will be most easily generated with \code{\link{quote}}
#' @param env The parent environment of the function, defaults to the calling
#'  environment of \code{make_function}
#' @export
#' @examples
#' f <- function(x) x + 3
#' g <- fn_new(alist(x = ), quote(x + 3))
#'
#' # The components of the functions are identical
#' identical(formals(f), formals(g))
#' identical(body(f), body(g))
#' identical(environment(f), environment(g))
#'
#' # But the functions are not identical because f has src code reference
#' identical(f, g)
#'
#' attr(f, "srcref") <- NULL
#' # Now they are:
#' stopifnot(identical(f, g))
fn_new <- function(args, body, env = parent.frame()) {
  stopifnot(all(has_names(args)), is_lang(body), is.environment(env))

  args <- as.pairlist(args)

  eval(call("function", args, body), env)
}

prim_eval <- eval(quote(sys.function(0)))
is_prim_eval <- function(x) identical(x, prim_eval)

# This predicate handles the fake primitive eval function produced
# when evaluating code with eval()
is_primitive <- function(x) {
  is.primitive(x) || is_prim_eval(x)
}

#' Name of a primitive function
#' @param prim A primitive function such as \code{base::c}.
prim_name <- function(prim) {
  stopifnot(is_primitive(prim))

  # Workaround because R_FunTab is not public
  name <- format(prim)
  name <- sub("^.Primitive\\(\"", "", name)
  name <- sub("\"\\)$", "", name)
  name
}

#' Extract arguments from a function
#'
#' \code{fn_defaults()} returns a named list of default
#' arguments. \code{fn_fmls_names()} returns the names of the
#' arguments.
#'
#' Contrarily to \code{formals()}, these helpers also work with
#' primitive functions.
#'
#' @param fn A function. It is lookep up in the calling frame if not
#'   supplied.
#' @seealso \code{\link{call_args}()} and
#'   \code{\link{call_args_names}()}
#' @export
#' @examples
#' # Extract from current call:
#' fn <- function(a = 1, b = 2) fn_fmls()
#' fn()
#'
#' # Works with primitive functions:
#' fn_fmls(base::switch)
fn_fmls <- function(fn = NULL) {
  fn <- fn %||% call_frame(2)$fn

  if (is_primitive(fn)) {
    fn_name <- prim_name(fn)
    if (fn_name == "eval") {
      # do_eval() starts a context with a fake primitive function as
      # function definition. We replace it here with the .Internal()
      # wrapper of eval() so we can match the arguments.
      fn <- base::eval
    } else {
      fn <- .ArgsEnv[[fn_name]] %||% .GenericArgsEnv[[fn_name]]
    }
  }

  formals(fn)
}

#' @rdname fn_fmls
#' @export
fn_fmls_names <- function(fn = NULL) {
  fn <- fn %||% call_frame(2)$fn
  args <- fn_fmls(fn)
  names(args)
}
