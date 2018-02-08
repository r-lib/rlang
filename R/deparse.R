
line_push <- function(line, text,
                      sticky = FALSE,
                      boundary = NULL,
                      width = NULL,
                      indent = 0L,
                      has_colour = FALSE) {
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

  if (!has_overflown(line, text, width, has_colour)) {
    return(paste0(line, text))
  }

  if (is_scalar_integer(boundary) && nchar(line) != boundary) {
    first <- substr(line, 0L, boundary)
    second <- substr(line, boundary + 1L, nchar(line))
    # Trim trailing spaces after boundary
    second <- trim_leading_spaces(second)
    second <- paste0(spaces(indent), second)
    if (sticky || !has_overflown(second, text, width, has_colour)) {
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
has_overflown <- function(line, text, width, has_colour) {
  if (has_colour) {
    line <- strip_style(line)
    text <- strip_style(text)
  }
  text <- trim_trailing_spaces(text)
  nchar(line) + nchar(text) > width && !is_spaces(line)
}
trim_trailing_spaces <- function(line) {
  sub(" *$", "", line)
}
trim_leading_spaces <- function(line) {
  sub("^ *", "", line)
}

new_lines <- function(width = peek_option("width"),
                      deparser = sexp_deparse) {
  width <- width %||% 60L

  r6lite(
    deparse = function(self, x) {
      deparser(x, lines = self)
    },

    width = width,
    boundary = NULL,
    next_sticky = FALSE,

    indent = 0L,
    indent_status = pairlist(),
    next_indent_sticky = FALSE,

    has_colour = FALSE,

    lines = chr(),
    last_line = chr(),

    get_lines = function(self) {
      c(self$lines, self$last_line)
    },
    get_indent = function(self) {
      if (self$indent < 0) {
        warn("Internal error: Negative indent while deparsing")
        0L
      } else {
        self$indent
      }
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
        indent = self$get_indent(),
        has_colour = self$has_colour
      )
      n <- length(line)

      if (n > 1) {
        self$lines <- c(self$lines, line[-n])
        self$last_line <- line[[n]]
        self$boundary <- NULL
        self$next_indent_sticky <- FALSE
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
      self$last_line <- spaces(self$get_indent())
      self$next_sticky <- FALSE
      self$next_indent_sticky <- FALSE
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
      self
    },
    set_boundary = function(self) {
      self$boundary <- nchar(self$last_line)
      self
    },

    increase_indent = function(self) {
      status <- node_car(self$indent_status)

      if (self$next_indent_sticky) {
        node_poke_cdr(status, inc(node_cdr(status)))
      } else {
        self$indent <- self$indent + 2L
        self$indent_status <- new_node(new_node(FALSE, 0L), self$indent_status)
        self$next_indent_sticky <- TRUE
      }

      self
    },
    decrease_indent = function(self) {
      status <- node_car(self$indent_status)
      if (is_null(status)) {
        warn("Internal error: Detected NULL `status` while deparsing")
        return(self)
      }

      reset <- node_car(status)
      n_sticky <- node_cdr(status)

      # Decrease indent level only once for all the openers that were
      # on a single line
      if (!reset) {
        self$indent <- self$indent - 2L
        node_poke_car(status, TRUE)
        self$next_indent_sticky <- FALSE
      }

      if (n_sticky >= 1L) {
        node_poke_cdr(status, dec(n_sticky))
      } else {
        self$indent_status <- node_cdr(self$indent_status)
        self$next_indent_sticky <- FALSE
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
      lines$deparse(node_car(x))
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

  lines$push_sticky(" ")
  lines$increase_indent()

  x <- node_cdr(x)
  lines$deparse(node_car(x))
  lines$decrease_indent()

  lines$get_lines()
}

fn_deparse <- function(x, lines) {
  lines$push("<function")

  fmls_deparse(fn_fmls(x), lines)

  lines$push_sticky(" ")
  lines$increase_indent()

  lines$deparse(body(x))
  lines$push_sticky(">")
  lines$decrease_indent()

  lines$get_lines()
}

while_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("while (")
  lines$deparse(node_car(x))

  x <- node_cdr(x)
  lines$push(") ")
  lines$deparse(node_car(x))

  lines$get_lines()
}
for_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("for (")
  lines$deparse(node_car(x))

  x <- node_cdr(x)
  lines$push(" in ")
  lines$deparse(node_car(x))

  x <- node_cdr(x)
  lines$push(") ")
  lines$deparse(node_car(x))

  lines$get_lines()
}
repeat_deparse <- function(x, lines = new_lines()) {
  lines$push("repeat ")
  lines$deparse(node_cadr(x))
  lines$get_lines()
}
if_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$push("if (")
  lines$deparse(node_car(x))

  x <- node_cdr(x)
  lines$push(") ")
  lines$deparse(node_car(x))

  x <- node_cdr(x)
  if (!is_null(x)) {
    lines$push(" else ")
    lines$deparse(node_car(x))
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

  lines$deparse(x)

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
  lines$deparse(node_cadr(x))
  lines$get_lines()
}

brackets_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$deparse(node_car(x))
  lines$push_sticky("[")
  lines$increase_indent()

  x <- node_cdr(x)
  lines$deparse(node_car(x))
  lines$push_sticky("]")
  lines$decrease_indent()

  lines$get_lines()
}
brackets2_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$deparse(node_car(x))
  lines$push_sticky("[[")
  lines$increase_indent()

  x <- node_cdr(x)
  lines$deparse(node_car(x))
  lines$push_sticky("]]")
  lines$decrease_indent()

  lines$get_lines()
}

parens_deparse <- function(x, lines = new_lines()) {
  lines$push("(")
  lines$deparse(node_cadr(x))
  lines$push(")")

  lines$get_lines()
}
braces_deparse <- function(x, lines = new_lines()) {
  lines$push("{")
  lines$increase_indent()

  x <- node_cdr(x)
  while (!is_null(x)) {
    lines$push_newline()
    lines$deparse(node_car(x))
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
    lines$deparse(node_car(x))

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
  lines$deparse(node_car(x))
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
    `[` = brackets_deparse,
    `[[` = brackets2_deparse,
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

atom_elements <- function(x) {
  elts <- as.character(x)

  na_pos <- are_na(x)
  elts[na_pos] <- "NA"

  elts[!na_pos] <- switch (typeof(x),
    integer = paste0(elts[!na_pos], "L"),
    character = paste0("\"", elts[!na_pos], "\""),
    elts[!na_pos]
  )

  elts
}
is_scalar_deparsable <- function(x) {
  if (typeof(x) == "raw" || length(x) != 1 || is_named(x)) {
    return(FALSE)
  }

  if (is_na(x) && !is_logical(x)) {
    return(FALSE)
  }

  TRUE
}

atom_deparse <- function(x, lines = new_lines()) {
  if (is_scalar_deparsable(x)) {
    lines$push(atom_elements(x))
    return(NULL)
  }

  truncated <- length(x) > 5L
  if (truncated) {
    x <- .subset(x, 1:5)
  }

  lines$push(paste0("<", rlang_type_sum(x), ": "))
  lines$increase_indent()

  elts <- atom_elements(x)
  nms <- names2(x)

  n <- length(elts)
  for (i in seq_len(n)) {
    nm <- nms[[i]]
    if (nzchar(nm)) {
      lines$push(paste0(nm, " = "))
      lines$make_next_sticky()
    }

    lines$push(elts[[i]])

    if (i != n) {
      lines$push_sticky(", ")
    }
  }

  if (truncated) {
    lines$push_sticky(", ")
    lines$push("...")
  }

  lines$push_sticky(">")
  lines$decrease_indent()

  lines$get_lines()
}

list_deparse <- function(x, lines = new_lines()) {
  lines$push(paste0("<list: "))
  lines$increase_indent()

  truncated <- length(x) > 5L
  if (truncated) {
    x <- .subset(x, 1:5)
  }

  nms <- names2(x)
  n <- length(x)

  for (i in seq_len(n)) {
    nm <- nms[[i]]
    if (nzchar(nm)) {
      lines$push(paste0(nm, " = "))
      lines$make_next_sticky()
    }

    lines$deparse(x[[i]])

    if (i != n) {
      lines$push_sticky(", ")
    }
  }

  if (truncated) {
    lines$push_sticky(", ")
    lines$push("...")
  }

  lines$push_sticky(">")
  lines$decrease_indent()

  lines$get_lines()
}

s3_deparse <- function(x, lines = new_lines()) {
  lines$push(paste0("<", rlang_type_sum(x), ">"))
  lines$get_lines()
}

literal_deparser <- function(type) {
  function(x, lines = new_lines()) {
    lines$push(paste0("<", type, ">"))
  }
}
default_deparse <- function(x, lines = new_lines()) {
  lines$push(deparse(x, control = "keepInteger"))
  lines$get_lines()
}

sexp_deparse <- function(x, lines = new_lines()) {
  if (is.object(x)) {
    s3_deparse(x, lines)
    return(NULL)
  }

  deparser <- switch (typeof(x),
    symbol = sym_deparse,
    language = call_deparser(x),
    closure = fn_deparse,
    `...` = literal_deparser("..."),
    any = literal_deparser("any"),
    environment = literal_deparser("environment"),
    externalptr = literal_deparser("pointer"),
    promise = literal_deparser("promise"),
    weakref = literal_deparser("weakref"),
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    raw = atom_deparse,
    list = list_deparse,
    default_deparse
  )
  deparser(x, lines)

  lines$get_lines()
}
