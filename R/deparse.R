
line_push <- function(line, text,
                      sticky = FALSE,
                      boundary = NULL,
                      width = NULL,
                      indent = 0L) {
  if (!length(line)) {
    return(text)
  }
  if (!is_string(line)) {
    abort("`line` must be a string or empty")
  }
  if (!is_string(text)) {
    abort("`text` must be a string")
  }
  width <- width %||% peek_option("width")

  if (!has_overflown(line, text, width)) {
    return(paste0(line, text))
  }

  if (is_scalar_integer(boundary) && nchar(line) != boundary) {
    first <- substr(line, 0L, boundary)
    second <- substr(line, boundary + 1L, nchar(line))
    # Trim trailing spaces after boundary
    second <- trim_leading_spaces(second)
    second <- paste0(spaces(indent), second)
    if (sticky || !has_overflown(second, text, width)) {
      line <- trim_trailing_spaces(first)
      text <- paste0(second, text)
    } else {
      text <- paste0(spaces(indent), text)
    }
  } else if (sticky) {
    line <- paste0(line, text)
    text <- chr()
  } else {
    line <- trim_trailing_spaces(line)
    text <- paste0(spaces(indent), text)
  }

  c(line, text)
}

spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}
is_spaces <- function(str) {
  identical(str, spaces(nchar(str)))
}
has_overflown <- function(line, text, width) {
  text <- trim_trailing_spaces(text)
  nchar(line) + nchar(text) > width && !is_spaces(line)
}
trim_trailing_spaces <- function(line) {
  sub(" *$", "", line)
}
trim_leading_spaces <- function(line) {
  sub("^ *", "", line)
}

new_lines <- function(width = peek_option("width")) {
  width <- width %||% 60L

  r6lite(
    width = width,
    indent = 0L,
    boundary = NULL,
    next_sticky = FALSE,

    lines = chr(),
    last_line = chr(),

    get_lines = function(self) {
      c(self$lines, self$last_line)
    },

    push = function(self, lines) {
      stopifnot(is_character(lines))
      for (line in lines) {
        self$push_one(line)
      }
      self
    },

    push_one = function(self, line) {
      line <- line_push(self$last_line, line,
        sticky = self$next_sticky,
        boundary = self$boundary,
        width = self$width,
        indent = self$indent
      )
      n <- length(line)

      if (n > 1) {
        self$lines <- c(self$lines, line[-n])
        self$last_line <- line[[n]]
        self$boundary <- NULL
      } else if (n) {
        self$last_line <- line
        if (self$next_sticky) {
          self$boundary <- nchar(line)
        }
      }
      self$next_sticky <- FALSE

      self
    },

    push_newline = function(self) {
      self$lines <- c(self$lines, self$last_line)
      self$last_line <- spaces(self$indent)
      self$next_sticky <- FALSE
      self
    },

    push_sticky = function(self, line) {
      stopifnot(is_string(line))
      self$next_sticky <- TRUE
      self$push(line)
      self$set_boundary()
      self
    },

    make_next_sticky = function(self) {
      self$next_sticky <- TRUE
    },

    set_boundary = function(self) {
      self$boundary <- nchar(self$last_line)
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

fmls_deparse <- function(x, lines = new_lines()) {
  lines$push_sticky("(")
  lines$increase_indent()

  while (!is_null(x)) {
    lines$push(as_string(node_tag(x)))

    car <- node_car(x)
    if (!is_missing(car)) {
      lines$push_sticky(" = ")
      lines$make_next_sticky()
      sexp_deparse(node_car(x), lines)
    }

    x <- node_cdr(x)
    if (!is_null(x)) {
      lines$push_sticky(", ")
    }
  }

  lines$push_sticky(")")
  lines$decrease_indent()

  lines$get_lines()
}
fn_call_deparse <- function(x, lines = new_lines()) {
  lines$push("function")

  x <- node_cdr(x)
  fmls_deparse(node_car(x), lines)

  lines$push(" ")

  x <- node_cdr(x)
  sexp_deparse(node_car(x), lines)

  lines$get_lines()
}

fn_deparse <- function(x, lines) {
  lines$push("<")
  lines$make_next_sticky()
  lines$push("function")

  fmls_deparse(fn_fmls(x), lines)
  lines$push(" ")

  sexp_deparse(body(x), lines)
  lines$push_sticky(">")

  lines$get_lines()
}

while_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("while (")
  sexp_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(") ")
  sexp_deparse(node_car(x), lines)

  lines$get_lines()
}
for_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("for (")
  sexp_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(" in ")
  sexp_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(") ")
  sexp_deparse(node_car(x), lines)

  lines$get_lines()
}
repeat_deparse <- function(x, lines = new_lines()) {
  lines$push("repeat ")
  sexp_deparse(node_cadr(x), lines)
  lines$get_lines()
}
if_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("if (")
  sexp_deparse(node_car(x), lines)

  x <- node_cdr(x)
  lines$push(") ")
  sexp_deparse(node_car(x), lines)

  x <- node_cdr(x)
  if (!is_null(x)) {
    lines$push(" else ")
    sexp_deparse(node_car(x), lines)
  }

  lines$get_lines()
}

# Wrap if the call lower in the AST is not supposed to have
# precedence. This sort of AST cannot arise in parsed code but can
# occur in constructed calls.
operand_deparse <- function(x, parent, side, lines) {
  wrap <- !call_has_precedence(x, parent, side)

  if (wrap) {
    lines$push("(")
    lines$make_next_sticky()
  }

  sexp_deparse(x, lines)

  if (wrap) {
    lines$push_sticky(")")
  }
}

binary_op_deparse <- function(x, lines = new_lines(), space = " ") {
  outer <- x;
  op <- as_string(node_car(x))

  x <- node_cdr(x)
  operand_deparse(node_car(x), outer, "lhs", lines)

  lines$push(paste0(space, op, space))

  x <- node_cdr(x)
  operand_deparse(node_car(x), outer, "rhs", lines)

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
  sexp_deparse(node_cadr(x), lines)
  lines$get_lines()
}

parens_deparse <- function(x, lines = new_lines()) {
  lines$push("(")
  sexp_deparse(node_cadr(x), lines)
  lines$push(")")

  lines$get_lines()
}
braces_deparse <- function(x, lines = new_lines()) {
  lines$push("{")
  lines$increase_indent()

  x <- node_cdr(x)
  while (!is_null(x)) {
    lines$push_newline()
    sexp_deparse(node_car(x), lines)
    x <- node_cdr(x)
  }

  lines$decrease_indent()
  lines$push_newline()
  lines$push("}")

  lines$get_lines()
}

sym_deparse <- function(x, lines = new_lines()) {
  lines$push(as_string(x))$get_lines()
}

args_deparse <- function(x, lines = new_lines()) {
  lines$push_sticky("(")
  lines$increase_indent()

  while (!is_null(x)) {
    tag <- node_tag(x)
    if (!is_null(tag)) {
      lines$push(as_string(tag))
      lines$push_sticky(" = ")
      lines$make_next_sticky()
    }
    sexp_deparse(node_car(x), lines)

    x <- node_cdr(x)
    if (!is_null(x)) {
      lines$push_sticky(", ")
    }
  }

  lines$push_sticky(")")
  lines$decrease_indent()

  lines$get_lines()
}
call_deparse <- function(x, lines = new_lines()) {
  sexp_deparse(node_car(x), lines)
  args_deparse(node_cdr(x), lines)
}

op_deparse <- function(op, x, lines) {
  deparser <- switch (op,
    `function` = fn_call_deparse,
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
call_deparser <- function(x) {
  op <- which_operator(x)
  if (op != "") {
    function(x, lines) op_deparse(op, x, lines)
  } else {
    call_deparse
  }
}

default_deparse <- function(x, lines = new_lines()) {
  lines$push(deparse(x, control = "keepInteger"))
  lines$get_lines()
}

sexp_deparse <- function(x, lines = new_lines()) {
  deparser <- switch (typeof(x),
    symbol = sym_deparse,
    language = call_deparser(x),
    closure = fn_deparse,
    default_deparse
  )
  deparser(x, lines)
}
