#' Display a call (or expression) as a tree.
#'
#' \code{ast_} takes a quoted expression; \code{ast} does the quoting
#' for you.
#'
#' @param x Quoted call, list of calls, or expression to display.
#' @param width Display width, defaults to current width as reported by
#'   \code{getOption("width")}.
#' @export
#' @examples
#' ast(f(x, 1, g(), h(i())))
#' ast(if (TRUE) 3 else 4)
#' ast(function(a = 1, b = 2) {a + b + 10})
#' ast(f(x)(y)(z))
#'
#' ast_(quote(f(x, 1, g(), h(i()))))
#' ast_(quote(if (TRUE) 3 else 4))
#' ast_(expression(1, 2, 3))
ast_ <- function(x, width = getOption("width")) {
  if (is.expression(x) || is.list(x)) {
    trees <- vapply(x, tree, character(1), width = width)
    out <- paste0(trees, collapse = "\n\n")
  } else {
    out <- tree(x, width = width)
  }

  cat(out, "\n")
}

#' @rdname ast_
#' @export
ast <- function(x) ast_(expr_find(x))

tree <- function(x, level = 1, width = getOption("width"), branch = "\u2517 ") {
  if (is_atomic(x) && length(x) == 1) {
    label <- paste0(" ", deparse(x)[1])
    children <- NULL
  } else if (is_name(x)) {
    x <- as.character(x)
    if (x == "") {
      # Special case the missing argument
      label <- "`MISSING"
    } else {
      label <- paste0("`", as.character(x))
    }

    children <- NULL
  } else if (is_call(x)) {
    label <- "()"
    children <-  vapply(as.list(x), tree, character(1),
      level = level + 1, width = width - 3)
  } else if (is_pairlist(x)) {
    label <- "[]"

    branches <- paste("\u2517", format(names(x)), "=")
    children <- character(length(x))
    for (i in seq_along(x)) {
      children[i] <- tree(x[[i]], level = level + 1, width = width - 3,
        branch = branches[i])
    }
  } else {
    # Special case for srcrefs, since they're commonly seen
    if (inherits(x, "srcref")) {
      label <- "<srcref>"
    } else {
      label <- paste0("<", typeof(x), ">")
    }
    children <- NULL
  }

  indent <- paste0(str_dup(" ", level - 1), branch)
  label <- str_trunc(label, width - 3)

  if (is.null(children)) {
    paste0(indent, label)
  } else {
    paste0(indent, label, "\n", paste0(children, collapse = "\n"))
  }
}

str_trunc <- function(x, width = getOption("width")) {
  ifelse(nchar(x) <= width, x, paste0(substr(x, 1, width - 3), "..."))
}

str_dup <- function(x, n) {
  paste0(rep(x, n), collapse = "")
}
