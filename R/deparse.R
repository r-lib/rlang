
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

binary_op_deparse <- function(x, lines = new_lines(), space = " ") {
  op <- as_string(node_car(x))

  x <- node_cdr(x)
  expr_deparse(node_car(x), lines)

  lines$push(paste0(space, op, space))

  x <- node_cdr(x)
  expr_deparse(node_car(x), lines)

  lines$lines
}
spaced_op_deparse <- function(x, lines = new_lines()) {
  binary_op_deparse(x, lines, space = " ")
}
unspaced_op_deparse <- function(x, lines = new_lines()) {
  binary_op_deparse(x, lines, space = "")
}

unary_op_deparse <- function(x, lines = new_lines()) {
  op <- as_string(node_car(x))
  lines$push(op)
  expr_deparse(node_cadr(x), lines)
  lines$lines
}

parens_deparse <- function(x, lines = new_lines()) {
  lines$push("(")
  expr_deparse(node_cadr(x), lines)
  lines$push(")")

  lines$lines
}
braces_deparse <- function(x, lines = new_lines()) {
  lines$push("{")
  lines$increase_indent()

  x <- node_cdr(x)
  while (!is_null(x)) {
    lines$push_new("")
    expr_deparse(node_car(x), lines)
    x <- node_cdr(x)
  }

  lines$decrease_indent()
  lines$push_new("}")

  lines$lines
}

default_deparse <- function(x, lines = new_lines()) {
  lines$push(deparse(x, control = "keepInteger"))
  lines$lines
}

op_deparse <- function(op, x, lines) {
  deparser <- switch (op,
    `while` = while_deparse,
    `for` = for_deparse,
    `if` = if_deparse,
    `?` = ,
    `<-` = ,
    `<<-` = ,
    `=` = ,
    `:=` = ,
    `~` = ,
    `|` = ,
    `||` = ,
    `&` = ,
    `&&` = ,
    `>` = ,
    `>=` = ,
    `<` = ,
    `<=` = ,
    `==` = ,
    `!=` = ,
    `+` = ,
    `-` = ,
    `*` = ,
    `/` = ,
    `%%` = ,
    `special` = spaced_op_deparse,
    `:` = ,
    `^` = ,
    `$` = ,
    `@` = ,
    `::` = ,
    `:::` = unspaced_op_deparse,
    `?unary` = ,
    `~unary` = ,
    `!` = ,
    `!!!` = ,
    `!!` = ,
    `+unary` = ,
    `-unary` =  unary_op_deparse,
    `(` = parens_deparse,
    `{` = braces_deparse,
    abort("Internal error: Unexpected operator while deparsing")
  )

  deparser(x, lines)
  lines$lines
}

expr_deparse <- function(x, lines = new_lines()) {
  op <- which_operator(x)
  if (op != "") {
    return(op_deparse(op, x, lines))
  }

  default_deparse(x, lines)
}
