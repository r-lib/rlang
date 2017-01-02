#' Close a temporary connection.
#'
#' \code{maybe_close()} closes connections but only if they have been
#' marked as temporary (see \code{\link{temporary}()}). It returns
#' \code{TRUE} if the connection was temporary and has been closed.
#'
#' @param conn A connection to close.
#' @export
#' @examples
#' path <- tempfile("my-file")
#' conn <- file(path)
#'
#' maybe_close(conn)
#' maybe_close(temporary(conn))
#' maybe_close(temporary(file(path)))
maybe_close <- function(conn) {
  if (is_temporary(conn)) {
    close(conn)
    TRUE
  } else {
    FALSE
  }
}
