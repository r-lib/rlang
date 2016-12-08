#' Read R code.
#'
#' These functions parse and transform text into R expressions. This
#' is the first step of interpreting or evaluating a piece of code
#' written by a programmer.
#'
#' \code{read()} returns one expression. If the text contains more
#' than one expression (separated by colons or new lines), an error is
#' issued. On the other hand \code{read_list()} can handle multiple
#' expressions. It always returns a list of expressions (compare to
#' \code{\link[base]{parse}()} which returns an obsolete expression
#' vector). Finally, \code{read_conn()} reads a R \link{connection}.
#'
#' The versions prefixed with \code{f_} return expressions quoted in
#' formulas rather than raw expressions.
#'
#' @param x Text containing expressions to read for \code{read()} and
#'   \code{read_list()}, or a connection to a file for
#'   \code{read_conn()}.
#' @param env The environment for the formulas. Defaults to the
#'   context in which the read function was called. Can be any object
#'   with a \code{as_env()} method.
#' @return \code{read()} returns a formula, \code{read_list()} and
#'   \code{read_conn()} return a list of formulas.
#' @seealso \code{\link[base]{parse}()}
#' @export
#' @examples
#' # read() can read any R expression:
#' read("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#'
#' # A string can contain several expressions separated by ; or \n
#' read_list("NULL; list()\n foo(bar)")
#'
#' # The versions prefixed with f_ return formulas:
#' f_read("foo %>% bar()")
#' f_read_list("1; 2; mtcars")
#'
#' # The env argument is passed to as_env(). It can be e.g. a string
#' # representing a scoped package environment:
#' f_read("identity(letters)", env = env_empty())
#' f_read_list("identity(letters); mtcars", env = "base")
read <- function(x) {
  exprs <- parse(text = read_validate(x))

  n <- length(exprs)
  if (n == 0) {
    stop("No expression to parse", call. = FALSE)
  } else if (n > 1) {
    stop("More than one expression parsed", call. = FALSE)
  }

  exprs[[1]]
}
#' @rdname read
#' @export
read_list <- function(x) {
  exprs <- parse(text = read_validate(x))
  as.list(exprs)
}
#' @rdname read
#' @export
read_conn <- function(x) {
  exprs <- parse(read_validate(x))
  as.list(exprs)
}
read_validate <- function(x) {
  if (!is_scalar_character(x)) {
    stop("Cannot read character vector of length > 1", call. = FALSE)
  }
  x
}

#' @rdname read
#' @export
f_read <- function(x, env = env_caller()) {
  f_new(read(x), env = as_env(env))
}
#' @rdname read
#' @export
f_read_list <- function(x, env = env_caller()) {
  lapply(read_list(x), f_new, env = as_env(env))
}
#' @rdname read
#' @export
f_read_conn <- function(x, env = env_caller()) {
  lapply(read_conn(x), f_new, env = as_env(env))
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
