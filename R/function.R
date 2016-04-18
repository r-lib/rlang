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
#' g <- function_new(alist(x = ), quote(x + 3))
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
function_new <- function(args, body, env = parent.frame()) {
  stopifnot(all(has_names(args)), is_lang(body), is.environment(env))

  args <- as.pairlist(args)

  eval(call("function", args, body), env)
}
