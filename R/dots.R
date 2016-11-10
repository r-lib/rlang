#' Extract dots from a frame
#'
#' \code{frame_dots_lsp()} returns a pairlist that is ready to be
#' spliced into a call, while \code{frame_dots()} returns a regular
#' list that is usually easier to work with.
#'
#' @param frame The environment from which the dots should be
#'   retrieved. Can be a frame, an environment, or a formula from
#'   which to retrieve an environment. If not supplied, the calling
#'   frame is used.
#' @export
frame_dots <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  dots <- frame_dots_lsp(frame)
  as.list(dots)
}

#' @rdname frame_dots
#' @export
frame_dots_lsp <- function(frame = NULL) {
  frame <- frame %||% call_frame(2)
  env <- get_env(frame)

  dots <- substitute(alist(...), env)
  dots <- cdr(dots)

  if (is.language(dots)) {
    NULL
  } else {
    dots
  }
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
dots_enumerate_argnames <- function(dots) {
  if (length(dots)) {
    names(dots) <- paste0("..", seq_along(dots))
  }
  dots
}

is_dot_nm <- function(nm) {
  grepl("\\.\\.[0-9]+$", nm)
}
