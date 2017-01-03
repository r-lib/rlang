#' Signal a dispatch failure.
#'
#' \code{dispatch_miss()} signals an aborting condition (see
#' \code{\link{cnd_abort}()} and \code{\link{abort}()}) of type
#' \code{dispatch_miss}. It also establishes a recovery restart named
#' \code{rst_redispatch}. This is intended to be used in tandem with
#' \code{\link{with_redispatch}()} with which a user can devise a
#' redispatch strategy in case method dispatch fails.
#'
#' @param .generic A string giving the name of the generic which
#'   failed to dispatch a method.
#' @param .object The object that was passed to \code{.generic}.
#' @param ... The additional arguments that were passed to
#'   \code{.generic}. These arguments may be passed on to another
#'   generic if a redispatch strategy is established on the
#'   stack.
#' @param .msg An error message for the user if dispatch fails (if no
#'   relevant generic redispatch is active). An error message similar
#'   to normal S3 failure is displayed by default.
#' @seealso \code{\link{with_redispatch}()} to establish a redispatch
#'   strategy. See \code{\link{hnd_redispatch}()} and
#'   \code{\link{rst_redispatch}()} to set up a redispatch handler or
#'   a redispatch restart manually.
#' @export
#' @examples
#' # To allow redispatching, the default method should signal a
#' # dispatch failure:
#' my_generic <- function(x, ...) UseMethod("my_generic")
#' my_generic.bar <- function(x, ...) "bar"
#' my_generic.default <- function(x, ...) {
#'   msg <- "dispatch failure."
#'   dispatch_miss("my_generic", x, ..., .msg = msg)
#' }
#'
#' foo_obj <- structure(NULL, class = "foo")
#'
#' # If the following line is executed, redispatch fails.  See
#' # with_redispatch() examples to set up a recovery mechanism.
#' \dontrun{
#' my_generic(foo_obj)
#' }
dispatch_miss <- function(.generic, .object, ..., .msg = NULL) {
  .msg <- .msg %||% dispatch_abort_msg(.generic, .object)

  with_restarts(rst_redispatch = rst_redispatch,
    cnd_abort(c("dispatch_miss", "dispatch", "error"), .msg = .msg,
      generic = .generic,
      object = .object,
      args = list(...)
    ))
}

dispatch_abort_msg <- function(generic, x) {
  classes <- paste0("`", paste0(class(x), collapse = "`, `"), "`")

  if (is_null(generic)) {
    generic <- ""
  } else {
    generic <- paste0("`", generic, "` ")
  }

  paste0("No ", generic, "method for object of type ", classes)
}


#' Restart dispatching.
#'
#' This restart function is established at signal site by
#' \code{\link{dispatch_miss}()}. It is normally called by
#' \code{\link{hnd_redispatch}()} but can be invoked manually with
#' \code{\link{rst_jump}("rst_redispatch")}.
#'
#' @param generic A generic function or a name of a such a function in
#'   scope from \code{env}. It is invoked to redispatch \code{object}.
#' @param object The object whose dispatching failed.
#' @param args Additional arguments passed on to \code{generic}.
#' @param env The environment in which \code{generic} should be
#'   evaluated.
#' @seealso \code{\link{dispatch_miss}()}, \code{\link{hnd_redispatch}()}
#' @export
rst_redispatch <- function(generic, object, args, env) {
  invoke(generic, c(list(object), args), .env = env)
}


#' Evaluate an expression with a redispatch strategy.
#'
#' These functions establish redispatch strategies. They require that
#' the default method signals a \code{dispatch_miss} condition (see
#' \code{\link{dispatch_miss}()}) when method dispatch fails. For
#' instance it is useful to try dispatching with one generic then
#' another if no method is available. An important limitation of this
#' method is that the function signature of the new generic should be
#' compatible with the signature of the failing generic.
#'
#' Technically, \code{with_redispatch()} establishes a restarting
#' handler (see \code{\link{restarting}()}) for conditions of type
#' \code{dispatch_miss}. These conditions should be signalled with
#' \code{\link{dispatch_miss}()}: this signalling function sets up a
#' recovery restart that provides a jump target for redispatched
#' generics. See \code{hnd_redispatch()} and \code{rst_redispatch()}
#' to set up a redispatch handler or restart manually.
#'
#' Note that while these functions were designed with S3 generics in
#' mind, they can be applied more generally to any function able to
#' take an object signalled with \code{\link{dispatch_miss}()} as
#' first argument.
#'
#' @inheritParams hnd_redispatch
#' @param .expr An expression to evaluate with redispatch enabled. The
#'   underscored version takes a quoted expression or a quoted
#'   formula.
#' @param .env The evaluation environment. The redispatching methods
#'   should be in scope in that environment. Defaults to the caller
#'   environment.
#' @param ...,.redispatch A dictionary of generics. The names specify
#'   the generics whose dispatching may fail, and the values should be
#'   other generic functions (or their names) to redispatch to.
#' @seealso \code{\link{dispatch_miss}()} for signalling a dispatch
#'   failure, and \code{\link{rst_redispatch}()} to set up a
#'   redispatching restart manually.
#' @export
#' @examples
#' # te default method for "my_generic" signals a dispatch failure:
#' my_generic <- function(x, ...) UseMethod("my_generic")
#' my_generic.bar <- function(x, ...) "bar"
#' my_generic.default <- function(x, ...) {
#'   msg <- "dispatch failure."
#'   dispatch_miss("my_generic", x, ..., .msg = msg)
#' }
#'
#' foo_obj <- structure(NULL, class = "foo")
#'
#' # If my_generic is called with a foo objects, redispatch fails:
#' \dontrun{
#' my_generic(foo_obj)
#' }
#'
#' # If foo is handled by another generic with a compatible argument
#' # signature, it can be redispatched to that generic:
#' other_generic <- function(x) UseMethod("other_generic")
#' other_generic.foo <- function(x) "foo"
#' with_redispatch(my_generic = other_generic, my_generic(foo_obj))
#'
#' # Note that it is important that my_generic() and other_generic()
#' # have a compatible signature, or at least are called in a
#' # compatible way. For instance other_generic does not accept
#' # additional arguments and will fail if not called appropriately:
#' \dontrun{
#' with_redispatch(my_generic = other_generic, my_generic(foo_obj, "arg"))
#' }
with_redispatch <- function(.expr, ..., .redispatch = list()) {
  expr <- arg_capture(.expr)
  .env <- env_caller()
  handler <- hnd_redispatch(..., .redispatch = .redispatch, .env = .env)
  with_handlers_(expr, list(dispatch_miss = handler))
}

#' @rdname with_redispatch
#' @export
with_redispatch_ <- function(.expr, ..., .redispatch = list(), .env = NULL) {
  expr <- as_quoted_f(.expr, .env)
  handler <- hnd_redispatch(..., .redispatch = .redispatch, .env = .env)
  with_handlers_(expr, list(dispatch_miss = handler))
}

#' @rdname with_redispatch
#' @export
hnd_redispatch <- function(..., .redispatch = list(), .env = env_caller()) {
  generics <- c(list(...), .redispatch)
  generics <- lapply_if(generics, is_scalar_character,
    get, envir = .env, mode = "function")
  stopifnot(all(vapply_lgl(generics, is_function)))

  inplace(function(c) {
    generic <- generics[[c$generic]]

    if (is_null(generic)) {
      msg <- dispatch_abort_msg(generic, c$object)
      abort(msg, c("dispatch_abort", "dispatch"))
    } else {
      rst_jump("rst_redispatch", generic, c$object, c$args, .env)
    }
  })
}
