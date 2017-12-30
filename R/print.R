
spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}
is_spaces <- function(str) {
  identical(str, spaces(nchar(str)))
}

push_line <- function(to, line, width = NULL, indent = 0L) {
  if (!is_string(line)) {
    abort("`line` must be a string")
  }
  if (!length(to)) {
    return(line)
  }
  n <- length(to)
  last <- to[[n]]

  width <- width %||% peek_option("width")

  if (nchar(last) + nchar(line) > width && !is_spaces(last)) {
    c(to, paste0(spaces(indent), line))
  } else {
    c(to[-n], paste0(to[[n]], line))
  }
}
push_lines <- function(to, lines, width = NULL, indent = 0L) {
  for (line in lines) {
    to <- push_line(to, line, width = width, indent = indent)
  }
  to
}
