
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

new_lines <- function(width = peek_option("width")) {
  width <- width %||% 60L

  new_r6lite(
    width = width,
    indent = 0L,

    lines = chr(),

    push = function(self, lines) {
      self$lines <- push_lines(self$lines, lines, self$width, self$indent)
    },
    push_new = function(self, line) {
      stopifnot(is_string(line))
      self$lines <- c(self$lines, paste0(spaces(self$indent), line))
    },

    increase_indent = function(self) {
      self$indent <- self$indent + 2L
    },
    decrease_indent = function(self) {
      self$indent <- self$indent - 2L
      if (self$indent < 0L) {
        warn("Internal issue: Negative indent detected")
        self$indent <- 0L
      }
    }
  )
}

while_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("while (")
  expr_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(") ")
  expr_deparse(node_car(x), lines)

  lines$lines
}
for_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("for (")
  expr_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(" in ")
  expr_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(") ")
  expr_deparse(node_car(x), lines)

  lines$lines
}
repeat_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("repeat ")
  expr_deparse(node_car(x), lines)

  x <- node_cdr(x)
  expr_deparse(node_car(x), lines)

  lines$lines
}
if_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("if (")
  expr_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(") ")
  expr_deparse(node_car(x), lines)

  x <- node_cdr(x)
  if (!is_null(x)) {
    lines$push(" else ")
    expr_deparse(node_car(x), lines)
  }

  lines$lines
}

braces_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("{")
  lines$increase_indent()

  while (!is_null(x)) {
    lines$push_new("")
    expr_deparse(node_car(x), lines)
    x <- node_cdr(x)
  }

  lines$decrease_indent()
  lines$push_new("}")

  lines$lines
}

expr_deparse <- function(x, lines = new_lines()) {
  switch (which_operator(x),
    `while` = while_deparse(x, lines),
    `for` = for_deparse(x, lines),
    `if` = if_deparse(x, lines),
    `{` = braces_deparse(x, lines),
    lines$push(deparse(x, control = "keepInteger"))
  )

  lines$lines
}
