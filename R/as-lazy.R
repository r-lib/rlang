#' Convert an object to a lazy object.
#'
#' @param x An R object. Current methods convert formulas, character
#'   vectors, calls and names.
#' @param env Environment to use for objects that don't already have
#'   associated environment.
#' @export
#' @examples
#' as.lazy(~ x + 1)
#' as.lazy(quote(x + 1), globalenv())
#' as.lazy("x + 1", globalenv())
as.lazy <- function(x, env) UseMethod("as.lazy")

#' @export
as.lazy.lazy <- function(x, env) x

#' @export
as.lazy.formula <- function(x, env) lazy_(x[[2]], environment(x))

#' @export
as.lazy.character <- function(x, env) lazy_(parse(text = x)[[1]], env)

#' @export
as.lazy.call <- function(x, env) lazy_(x, env)

#' @export
as.lazy.name <- function(x, env) lazy_(x, env)
