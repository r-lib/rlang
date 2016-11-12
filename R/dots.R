#' Extract dots
#'
#' \code{frame_dots()} extracts dots from a frame and
#' \code{arg_dots()} extracts dots from its arguments. The
#' \code{_lsp()} versions return a pairlist that is ready to be
#' spliced into a call, while the regular versions return a regular
#' list that is usually easier to work with.
#'
#' \code{frame_dots()} and \code{frame_dots_lsp()} never fail, even if
#' the frame does not contain dots. Instead they return an empty list
#' or \code{NULL} respectively.
#'
#' @param frame The environment from which the dots should be
#'   retrieved. Can be a frame, an environment, or a formula from
#'   which to retrieve an environment. If not supplied, the calling
#'   frame is used.
#' @param ... Arguments to extract. Can be both forwarded dots and
#'   direct arguments.
#' @export
frame_dots <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  as.list(frame_dots_lsp(frame))
}

#' @rdname frame_dots
#' @export
frame_dots_lsp <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  env <- get_env(frame)

  dots <- cdr(substitute(alist(...), env))
  if (is.language(dots)) {
    NULL
  } else {
    dots
  }
}

#' @rdname frame_dots
#' @export
arg_dots <- function(...) {
  eval(substitute(alist(...)))
}

#' @rdname frame_dots
#' @export
arg_dots_lsp <- function(...) {
  dots <- cdr(substitute(alist(...)))
  if (is.language(dots)) {
    NULL
  } else {
    dots
  }
}

#' Inspect dots
#'
#' Runs \code{\link{arg_info}()} for each dots element, and return the
#' results in a list.
#'
#' \code{dots_info_()} is the standard evaluation version of
#' \code{dots_info()} and takes a list of dots as captured by
#' \code{\link{frame_dots}()} or \code{\link{arg_dots}()}, and a call
#' stack as returned by \code{\link{call_stack}()}.
#'
#' @param ... Dots to inspect.
#' @seealso \code{\link{arg_info}()}
#' @export
dots_info <- function(...) {
  dots <- arg_dots(...)
  stack <- call_stack()
  dots_info_(dots, stack)
}

#' @rdname dots_info
#' @inheritParams arg_info_
#' @param dots Dots to inspect.
#' @export
dots_info_ <- function(dots, stack) {
  dots_syms <- dots_enumerate_sym(dots)
  dots_syms <- set_names(dots_syms, names(dots))
  lapply(dots_syms, arg_info_, stack)
}

dots_enumerate_sym <- function(dots) {
  nms <- paste0("..", seq_along(dots))
  lapply(nms, as.name)
}
dots_enumerate_args <- function(dots) {
  i <- 1
  lsp_walk(dots, function(dot) {
    dot_name <- as.symbol(paste0("..", i))
    set_car(dot, dot_name)
    i <<- i + 1
  })
  dots
}

is_dot_nm <- function(nm) {
  grepl("\\.\\.[0-9]+$", nm)
}
