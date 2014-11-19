#' Automatically name all components of a lazy dots.
#'
#' Any components missing a name will automatically get a name added by
#' looking at the first \code{max_width} characters of the deparsed expression.
#'
#' @param x A \code{\link{lazy_dots}}
#' @param max_width Maximum number of characters to use
#' @keywords internal
#' @export
#' @examples
#' x <- lazy_dots(1 + 2, mean(mpg))
#' auto_name(x)
#'
#' auto_name(list(~f, quote(x)))
auto_name <- function(x, max_width = 40) {
  names(x) <- auto_names(x, max_width = max_width)
  x
}

auto_names <- function(x, max_width = 40) {
  x <- as.lazy_dots(x)

  nms <- names(x) %||% rep("", length(x))

  missing <- nms == ""
  expr <- lapply(x[missing], `[[`, "expr")
  nms[missing] <- vapply(expr, deparse_trunc, width = max_width,
    FUN.VALUE = character(1), USE.NAMES = FALSE)

  nms
}

deparse_trunc <- function(x, width = getOption("width")) {
  if (is.symbol(x)) {
    return(as.character(x))
  }

  text <- deparse(x, width.cutoff = width)
  if (length(text) == 1 && nchar(text) < width) return(text)

  paste0(substr(text[1], 1, width - 3), "...")
}

