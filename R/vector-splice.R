#' Splice objects and lists of objects into a list
#'
#' This splices all arguments into a list. Non-list objects and lists
#' with a S3 class are encapsulated in a list before concatenation.
#' @param ... Objects to concatenate.
#' @export
#' @examples
#' inputs <- list(arg1 = "a", arg2 = "b")
#'
#' # splice() concatenates the elements of inputs with arg3
#' str(splice(inputs, arg3 = c("c1", "c2")))
#' str(list(inputs, arg3 = c("c1", "c2")))
#' str(c(inputs, arg3 = c("c1", "c2")))
splice <- function(...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(dots)
  }

  names <- Map(function(dot, name) {
    if (is_bare_list(dot)) {
      names2(dot)
    } else {
      name
    }
  }, dots, names2(dots))
  names <- unlist(names, recursive = FALSE)

  is_not_list <- map_lgl(dots, function(x) !is_bare_list(x))
  dots[is_not_list] <- map(dots[is_not_list], list)

  out <- unlist(dots, recursive = FALSE)
  set_names(out, names)
}

