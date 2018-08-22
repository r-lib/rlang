#' Execute a function
#'
#' @description
#' This function constructs and evaluates a call to `f`.
#' It was two primary uses:
#'
#' * To call a function with arguments stored in a list (if the function
#'   doesn't support [tidy-dots])
#'
#' * To call every function stored in a list (in conjunction with `map()`/
#'   [lapply()])
#'
#' @param f A function, or function name as a string.
#' @param ... Arguments to function.
#'
#'   These dots support [tidy-dots] features.
#' @param  .env Environment in which to evaluate the call. This will be
#'   most useful if `f` is a string, or the function has side-effects.
#' @export
#' @examples
#' args <- list(x = c(1:10, 100, NA), na.rm = TRUE)
#' exec("mean", !!!args)
#' exec("mean", !!!args, trim = 0.2)
#'
#' fs <- list(a = function() "a", b = function() "b")
#' lapply(fs, exec)
#'
#' # Compare to do.call it will not automatically inline expressions
#' # into the evaluated call.
#' x <- 10
#' args <- exprs(x1 = x + 1, x2 = x * 2)
#' exec(list, !!!args)
#' do.call(list, args)
#'
#' # exec() is not designed to generate pretty function calls. This is
#' # most easily seen if you call a function that captures the call:
#' f <- disp ~ cyl
#' exec("lm", f, data = mtcars)
#'
#' # If you need finer control over the generated call, you'll need to
#' # construct it yourself. This may require creating a new environment
#' # with carefully constructed bindings
#' data_env <- env(data = mtcars)
#' eval(expr(lm(!!f, data)), data_env)
exec <- function(f, ..., .env = caller_env()) {
  args <- list2(...)
  args <- map(args, sym_protect)

  do.call(f, args, envir = .env)
}

sym_protect <- function(x) {
  if (is_symbolic(x)) {
    call2(quote, x)
  } else {
    x
  }
}

