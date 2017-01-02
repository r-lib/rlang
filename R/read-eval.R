#' Parse R code.
#'
#' These functions parse and transform text into R expressions. This
#' is the first step to interpret or evaluate a piece of R code
#' written by a programmer.
#'
#' \code{parse_expr()} returns one expression. If the text contains
#' more than one expression (separated by colons or new lines), an
#' error is issued. On the other hand \code{parse_exprs()} can handle
#' multiple expressions. It always returns a list of expressions
#' (compare to \code{\link[base]{parse}()} which returns an
#' \link[base]{expression} vector). All functions also support R
#' connections.
#'
#' The versions prefixed with \code{f_} return expressions quoted in
#' formulas rather than raw expressions.
#'
#' @param x Text containing expressions to parse_expr for
#'   \code{parse_expr()} and \code{parse_exprs()}. Can also be an R
#'   connection, for instance to a file. If the connection is marked
#'   as temporary (see \code{\link{temporary}()}), it will be
#'   automatically closed.
#' @param env The environment for the formulas. Defaults to the
#'   context in which the parse_expr function was called. Can be any object
#'   with a \code{as_env()} method.
#' @return \code{parse_expr()} returns a formula, \code{parse_exprs()}
#'   returns a list of formulas.
#' @seealso \code{\link[base]{parse}()}
#' @export
#' @examples
#' # parse_expr() can parse_expr any R expression:
#' parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#'
#' # A string can contain several expressions separated by ; or \n
#' parse_exprs("NULL; list()\n foo(bar)")
#'
#' # The versions prefixed with f_ return formulas:
#' f_parse_expr("foo %>% bar()")
#' f_parse_exprs("1; 2; mtcars")
#'
#' # The env argument is passed to as_env(). It can be e.g. a string
#' # representing a scoped package environment:
#' f_parse_expr("identity(letters)", env = env_empty())
#' f_parse_exprs("identity(letters); mtcars", env = "base")
#'
#'
#' # You can also parse source files by passing a R connection. Let's
#' # create a file containing R code:
#' path <- tempfile("my-file.R")
#' cat("1; 2; mtcars", file = path)
#'
#' # This file can be parsed by first opening a connection.
#' # The connection needs to be closed afterwards.
#' conn <- file(path)
#' parse_exprs(conn)
#' close(conn)
#'
#' # You can also signal that the connection should be closed
#' # automatically by supplying a temporary connection:
#' parse_exprs(temporary(file(path)))
parse_expr <- function(x) {
  exprs <- parse_exprs(x)

  n <- length(exprs)
  if (n == 0) {
    stop("No expression to parse_expr", call. = FALSE)
  } else if (n > 1) {
    stop("More than one expression parsed_expr", call. = FALSE)
  }

  exprs[[1]]
}
#' @rdname parse_expr
#' @export
parse_exprs <- function(x) {
  if (inherits(x, "connection")) {
    exprs <- parse(file = x)
    maybe_close(x)
  } else if (is_scalar_character(x)) {
    exprs <- parse(text = x)
  } else {
    abort("`x` must be a string or a R connection")
  }
  as.list(exprs)
}

#' @rdname parse_expr
#' @export
f_parse_expr <- function(x, env = env_caller()) {
  f_new(parse_expr(x), env = as_env(env))
}
#' @rdname parse_expr
#' @export
f_parse_exprs <- function(x, env = env_caller()) {
  lapply(parse_exprs(x), f_new, env = as_env(env))
}


#' Invoke a function with a list of arguments.
#'
#' Normally, you invoke a R function by typing arguments manually. A
#' powerful alternative is to call a function with a list of arguments
#' assembled programmatically. This is the purpose of \code{invoke()}.
#'
#' \code{invoke()} is basically a version of
#' \code{\link[base]{do.call}()} that creates cleaner call traces
#' because it does not inline the function and the arguments in the
#' call (see examples). To achieve this, \code{invoke()} creates a
#' child environment of \code{.env} with \code{.fn} and all arguments
#' bound to new symbols (see \code{\link{env_bury}()}). It then builds
#' a call with those symbols and evaluates it in a forged promise (see
#' \code{\link{with_env}()}).
#'
#' @param .fn A function to invoke. Can be a function object or the
#'   name of a function in scope of \code{.env}.
#' @param .args,... List of arguments (possibly named) to be passed to
#'   \code{.fn}.
#' @param .env The environment in which to call \code{.fn}.
#' @param .bury A character vector of length 2. The first string
#'   specifies which name should the function have in the call
#'   recorded in the evaluation stack. The second string specifies a
#'   prefix for the argument names. Set \code{.bury} to FALSE if you
#'   prefer to inline the function and its arguments in the call.
#' @export
#' @examples
#' # invoke() has the same purpose as do.call():
#' invoke(paste, letters)
#'
#' # But it creates much cleaner calls:
#' invoke(call_inspect, mtcars)
#'
#' # and stacktraces:
#' fn <- function(...) sys.calls()
#' invoke(fn, list(mtcars))
#'
#' # Compare to do.call():
#' do.call(call_inspect, mtcars)
#' do.call(fn, list(mtcars))
#'
#'
#' # Specify the function name either by supplying a string
#' # identifying the function (it should be visible in .env):
#' invoke("call_inspect", letters)
#'
#' # Or by changing the .bury argument, with which you can also change
#' # the argument prefix:
#' invoke(call_inspect, mtcars, .bury = c("inspect!", "col"))
invoke <- function(.fn, .args = list(), ...,
                   .env = env_caller(), .bury = c(".fn", "")) {
  args <- c(.args, list(...))

  if (is_false(.bury) || !length(args)) {
    # Evaluate with a promise rather than do.call() to keep eval stack clean
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- as.call(c(.fn, args))
    env_assign_lazily_(env(), "promise", call, .env)
    return(promise)
  }


  if (!is_character(.bury, 2)) {
    stop(".bury must be a character vector of length 2", call. = FALSE)
  }
  arg_prefix <- .bury[[2]]
  fn_nm <- .bury[[1]]

  buried_nms <- paste0(arg_prefix, seq_along(.args))
  buried_args <- set_names(.args, buried_nms)
  .env <- env_bury(.env, buried_args)
  .args <- set_names(buried_nms, names(.args))
  .args <- lapply(.args, as.name)

  if (is_function(.fn)) {
    env_assign(.env, fn_nm, .fn)
    .fn <- fn_nm
  }

  call <- as.call(c(as_name(.fn), .args))
  env_assign_lazily_(env(), "promise", call, .env)
  promise
}
