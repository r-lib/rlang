#' Create a call by "hand"
#'
#' @param f Function to call. For \code{make_call}, either a string, a symbol
#'   or a quoted call. For \code{do_call}, a bare function name or call.
#' @param ...,.args Arguments to the call either in or out of a list
#' @export
#' @examples
#' # f can either be a string, a symbol or a call
#' new_call("f", a = 1)
#' new_call(quote(f), a = 1)
#' new_call(quote(f()), a = 1)
#'
#' #' Can supply arguments individually or in a list
#' new_call(quote(f), a = 1, b = 2)
#' new_call(quote(f), .args = list(a = 1, b = 2))
new_call <- function(f, ..., .args = list()) {
  if (is.character(f)) {
    if (length(f) != 1) {
      stop("Character `f` must be length 1", call. = FALSE)
    }
    f <- as.name(f)
  }

  args <- c(list(...), as.list(.args))
  as.call(c(f, args))
}


#' Create a formula object by "hand".
#'
#' @param lhs,rhs A call, name, or atomic vector.
#' @param env An environment
#' @return A formula object
#' @export
#' @examples
#' new_formula(quote(a))
#' new_formula(quote(a), quote(b))
new_formula <- function(rhs, lhs = NULL, env = parent.frame()) {
  if (!is_lang(rhs)) {
    stop("`rhs` is not a valid language object", call. = FALSE)
  }
  if (!is_lang(lhs) && !is.null(lhs)) {
    stop("`lhs` is not a valid language object", call. = FALSE)
  }
  if (!is.environment(env)) {
    stop("`env` is not an environment", call. = FALSE)
  }

  if (is.null(lhs)) {
    f <- new_call("~", rhs)
  } else {
    f <- new_call("~", lhs, rhs)
  }

  structure(
    f,
    class = "formula",
    .Environment = env
  )
}


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
#' g <- new_function(alist(x = ), quote(x + 3))
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
new_function <- function(args, body, env = parent.frame()) {
  stopifnot(all(has_names(args)), is_lang(body))

  args <- as.pairlist(args)
  env <- to_env(env)

  eval(call("function", args, body), env)
}
