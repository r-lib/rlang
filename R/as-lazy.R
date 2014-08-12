#' @export
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
