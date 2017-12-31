
spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}
is_spaces <- function(str) {
  identical(str, spaces(nchar(str)))
}

merge_lines <- function(last, line, width = NULL, indent = 0L) {
  if (!length(last)) {
    return(line)
  }
  if (!is_string(last)) {
    abort("`last` must be a string or empty")
  }
  if (!is_string(line)) {
    abort("`line` must be a string")
  }
  width <- width %||% peek_option("width")

  if (nchar(last) + nchar(line) > width && !is_spaces(last)) {
    c(last, paste0(spaces(indent), line))
  } else {
    paste0(last, line)
  }
}

new_lines <- function(width = peek_option("width")) {
  width <- width %||% 60L

  r6lite(
    width = width,
    indent = 0L,

    lines = chr(),
    last_line = chr(),
    lazy_line = chr(),

    get_lines = function(self) {
      c(self$lines, self$last_line)
    },

    push = function(self, line) {
      stopifnot(is_string(line))

      last <- merge_lines(self$last_line, line, self$width, self$indent)
      n <- length(last)

      if (n > 1) {
        self$flush()
        self$lines <- c(self$lines, last[-n])
        self$last_line <- last[[n]]
      } else if (n) {
        self$last_line <- last
      }

      self
    },

    push_newline = function(self) {
      self$flush()
      self$lines <- c(self$lines, self$last_line)
      self$last_line <- spaces(self$indent)
      self
    },

    # We stage the sticky elements so we can compute their width
    # properly before actually pushing them to `lines`
    push_lazy_line = function(self, line) {
      stopifnot(
        is_string(line),
        length(self$lazy_line) %in% 0:1
      )
      self$lazy_line <- paste0(self$lazy_line, line)
      self
    },

    make_last_line_lazy = function(self) {
      if (length(self$lazy_line)) {
        abort("Internal error: Tried to overwrite lazy line")
      }

      last <- self$last_line
      if (length(last)) {
        last <- gsub("^ *", "", last)
        self$last_line <- chr()
        self$lazy_line <- last
      }

      self
    },

    # Flush the `lazy_line` that is currently staged, if any
    flush = function(self) {
      lazy <- self$lazy_line

      # Avoid recursion within self$push()
      self$lazy_line <- chr()
      if (length(lazy)) {
        self$push(lazy)
      }

      self
    },

    increase_indent = function(self) {
      self$indent <- self$indent + 2L
      self
    },
    decrease_indent = function(self) {
      self$indent <- self$indent - 2L
      if (self$indent < 0L) {
        warn("Internal problem: Negative indent detected")
        self$indent <- 0L
      }
      self
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

  lines$get_lines()
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

  lines$get_lines()
}
repeat_deparse <- function(x, lines = new_lines()) {
  lines$push("repeat ")
  expr_deparse(node_cadr(x), lines)
  lines$get_lines()
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

  lines$get_lines()
}

binary_op_deparse <- function(x, lines = new_lines(), space = " ") {
  op <- as_string(node_car(x))

  x <- node_cdr(x)
  expr_deparse(node_car(x), lines)

  lines$push(paste0(space, op, space))

  x <- node_cdr(x)
  expr_deparse(node_car(x), lines)

  lines$get_lines()
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
  lines$get_lines()
}

parens_deparse <- function(x, lines = new_lines()) {
  lines$push("(")
  expr_deparse(node_cadr(x), lines)
  lines$push(")")

  lines$get_lines()
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

  lines$get_lines()
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

  lines$get_lines()
}

default_deparse <- function(x, lines = new_lines()) {
  lines$push(deparse(x, control = "keepInteger"))
  lines$get_lines()
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
  lines$get_lines()
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
