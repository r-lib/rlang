
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

  r6lite(
    width = width,
    indent = 0L,

    lines = chr(),
    push = function(self, lines) {
      self$flush()
      self$lines <- push_lines(self$lines, lines, self$width, self$indent)
    },
    push_newline = function(self) {
      self$flush()
      self$lines <- c(self$lines, spaces(self$indent))
    },

    # We stage the sticky elements so we can compute their width
    # properly before actually pushing them to `lines`
    lazy_line = chr(),
    push_lazy_line = function(self, line) {
      stopifnot(
        is_string(line),
        length(self$lazy_line) %in% 0:1
      )
      if (!length(self$lazy_line)) {
        self$lazy_line <- spaces(self$indent)
      }
      self$lazy_line <- paste0(self$lazy_line, line)
    },

    make_last_line_lazy = function(self) {
      n <- length(self$lines)
      if (n) {
        last <- self$lines[[n]]
        self$lines <- self$lines[seq_len(n - 1)]
        self$lazy_line <- last
      }
    },

    # Flush the `lazy_line` that is currently staged, if any
    flush = function(self) {
      if (length(self$lazy_line)) {
        self$lines <- push_lines(self$lines, self$lazy_line, self$width, self$indent)
        self$lazy_line <- chr()
      }
    },

    increase_indent = function(self) {
      self$indent <- self$indent + 2L
    },
    decrease_indent = function(self) {
      self$indent <- self$indent - 2L
      if (self$indent < 0L) {
        warn("Internal problem: Negative indent detected")
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
  lines$push("repeat ")
  expr_deparse(node_cadr(x), lines)
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
    lines$push_newline()
    expr_deparse(node_car(x), lines)
    x <- node_cdr(x)
  }

  lines$decrease_indent()
  lines$push_newline()
  lines$push("}")

  lines$lines
}

sym_deparse <- function(x, lines = new_lines()) {
  lines$push(as_string(x))$lines
}

call_deparse <- function(x, lines = new_lines()) {
  expr_deparse(node_car(x), lines)
  lines$push("(")
  lines$increase_indent()

  x <- node_cdr(x)
  while (!is_null(x)) {
    expr_deparse(node_car(x), lines)
    x <- node_cdr(x)
    if (!is_null(x)) {
      lines$push(", ")
    }
  }

  lines$push(")")
  lines$decrease_indent()
  lines$lines
}

default_deparse <- function(x, lines = new_lines()) {
  lines$push(deparse(x, control = "keepInteger"))$lines
}

op_deparse <- function(op, x, lines) {
  deparser <- switch (op,
    `while` = while_deparse,
    `for` = for_deparse,
    `repeat` = repeat_deparse,
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

  deparser <- switch (typeof(x),
    symbol = sym_deparse,
    language = call_deparse,
    default_deparse
  )
  deparser(x, lines)
}
