line_push <- function(line, text,
                      sticky = FALSE,
                      boundary = NULL,
                      width = NULL,
                      indent = 0L,
                      has_colour = FALSE) {
  if (!length(line)) {
    return(text)
  }
  check_string(line)
  check_string(text)

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

spaces <- function(ns) {
  map_chr(ns, function(n) paste(rep(" ", n), collapse = ""))
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
                      max_elements = 5L,
                      deparser = sexp_deparse) {
  width <- width %||% 60L
  stopifnot(
    is_integerish(width, n = 1),
    is_null(max_elements) || is_scalar_integerish(max_elements)
  )

  r6lite(
    deparse = function(self, x) {
      deparser(x, lines = self)
    },

    width = width,
    max_elements = max_elements,
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
        node_poke_cadr(status, inc(node_cadr(status)))
      } else {
        self$indent <- self$indent + 2L
        self$indent_status <- new_node(new_node(FALSE, new_node(0L, NULL)), self$indent_status)
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
      n_sticky <- node_cadr(status)

      # Decrease indent level only once for all the openers that were
      # on a single line
      if (!reset) {
        self$indent <- self$indent - 2L
        node_poke_car(status, TRUE)
        self$next_indent_sticky <- FALSE
      }

      if (n_sticky >= 1L) {
        node_poke_cadr(status, dec(n_sticky))
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
    sym_deparse(node_tag(x), lines)

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
next_deparse <- function(x, lines = new_lines()) {
  lines$push("next")
  lines$get_lines()
}
break_deparse <- function(x, lines = new_lines()) {
  lines$push("break")
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

binary_op_deparse <- function(x, lines = new_lines(), space = " ", sticky_rhs = FALSE) {
  # Constructed call without second argument
  if (is_null(node_cddr(x))) {
    return(call_deparse(x, lines))
  }

  outer <- x;
  op <- as_string(node_car(x))

  x <- node_cdr(x)
  operand_deparse(node_car(x), outer, "lhs", lines)

  lines$push_sticky(paste0(space, op, space))

  if (sticky_rhs) {
    lines$make_next_sticky()
  }

  x <- node_cdr(x)

  lines$increase_indent()
  operand_deparse(node_car(x), outer, "rhs", lines)
  lines$decrease_indent()

  lines$get_lines()
}
spaced_op_deparse <- function(x, lines = new_lines()) {
  binary_op_deparse(x, lines, space = " ")
}
unspaced_op_deparse <- function(x, lines = new_lines()) {
  binary_op_deparse(x, lines, space = "")
}
tight_op_deparse <- function(x, lines = new_lines()) {
  binary_op_deparse(x, lines, space = "", sticky_rhs = TRUE)
}

unary_op_deparse <- function(x, lines = new_lines()) {
  # Constructed call without argument
  if (is_null(node_cdr(x))) {
    return(call_deparse(x, lines))
  }

  op <- as_string(node_car(x))
  lines$push(op)
  lines$deparse(node_cadr(x))
  lines$get_lines()
}
unary_f_deparse <- function(x, lines = new_lines()) {
  # Constructed call without argument
  if (is_null(node_cdr(x))) {
    return(call_deparse(x, lines))
  }

  lines$push("~")

  rhs <- node_cadr(x)
  if (!is_symbol(rhs) && !is_syntactic_literal(rhs)) {
    lines$push(" ")
  }
  lines$deparse(rhs)

  lines$get_lines()
}

brackets_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$deparse(node_car(x))

  args_deparse(node_cdr(x), lines, delims = c("[", "]"))

  lines$get_lines()
}
brackets2_deparse <- function(x, lines = new_lines()) {
  x <- node_cdr(x)
  lines$deparse(node_car(x))

  args_deparse(node_cdr(x), lines, delims = c("[[", "]]"))

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

  # No need for a newline if the block is empty
  if (is_null(x)) {
    lines$push(" }")
    return(lines$get_lines())
  }

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
  str <- encodeString(as_string(x))

  if (needs_backticks(str)) {
    str <- sprintf("`%s`", str)
  }

  lines$push(str)$get_lines()
}

args_deparse <- function(x, lines = new_lines(), delims = c("(", ")")) {
  stopifnot(is_character(delims, n = 2))

  lines$push_sticky(delims[[1]])
  lines$increase_indent()

  while (!is_null(x)) {
    tag <- node_tag(x)
    if (!is_null(tag)) {
      sym_deparse(tag, lines = lines)
      lines$push_sticky(" = ")
      lines$make_next_sticky()
    }
    lines$deparse(node_car(x))

    x <- node_cdr(x)
    if (!is_null(x)) {
      lines$push_sticky(", ")
    }
  }

  lines$push_sticky(delims[[2]])
  lines$decrease_indent()

  lines$get_lines()
}
call_deparse <- function(x, lines = new_lines()) {
  car <- node_car(x)

  type <- call_delimited_type(car)
  switch(type,
    parens = {
      car <- call("(", car)
      lines$deparse(car)
    },
    backticks = {
      lines$deparse(node_car(car))
      args_deparse(node_cdr(car), lines)
    },
    lines$deparse(car)
  )

  args_deparse(node_cdr(x), lines)
}

call_delimited_type <- function(call) {
  if (!is_call(call)) {
    return("none")
  }

  op <- call_parse_type(call)
  if (op == "") {
    return("none")
  }

  switch (op,
    `function` =
      "parens",
    `while` = ,
    `for` = ,
    `repeat` = ,
    `if` = ,
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
    `special` = ,
    `:` = ,
    `^` = ,
    `?unary` = ,
    `~unary` = ,
    `!` = ,
    `!!!` = ,
    `!!` = ,
    `+unary` = ,
    `-unary` =
      "backticks",
    `$` = ,
    `@` = ,
    `::` = ,
    `:::` = ,
    `[` = ,
    `[[` = ,
    `(` = ,
    `{` =
      "none",
    abort("Internal error: Unexpected operator while deparsing")
  )
}

op_deparse <- function(op, x, lines) {
  deparser <- switch (op,
    `function` = fn_call_deparse,
    `while` = while_deparse,
    `for` = for_deparse,
    `repeat` = repeat_deparse,
    `if` = if_deparse,
    `next` = next_deparse,
    `break` = break_deparse,
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
    `@` = unspaced_op_deparse,
    `::` = ,
    `:::` = tight_op_deparse,
    `~unary` = unary_f_deparse,
    `?unary` = ,
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
  op <- call_parse_type(x)
  if (op != "") {
    function(x, lines) op_deparse(op, x, lines)
  } else {
    call_deparse
  }
}

atom_elements <- function(x) {
  elts <- as.character(x)

  na_pos <- are_na(x) & !is.nan(x)
  elts[na_pos] <- "NA"

  elts[!na_pos] <- switch (typeof(x),
    integer = paste0(elts[!na_pos], "L"),
    character = map_chr(elts[!na_pos], deparse),
    elts[!na_pos]
  )

  elts
}
is_scalar_deparsable <- function(x) {
  typeof(x) != "raw" && length(x) == 1 && !is_named(x)
}

atom_deparse <- function(x, lines = new_lines()) {
  if (is_scalar_deparsable(x)) {
    lines$push(deparse(x))
    return(NULL)
  }

  max_elements <- lines$max_elements

  truncated <- !is.null(max_elements) && length(x) > max_elements
  if (truncated) {
    x <- .subset(x, seq_len(max_elements))
  }

  lines$push(paste0("<", rlang_type_sum(x), ": "))
  lines$increase_indent()

  elts <- atom_elements(x)
  nms <- deparsed_names(x)

  n <- length(elts)
  for (i in seq_len(n)) {
    nm <- nms[[i]]
    if (nzchar(nm)) {
      lines$push(paste0(nm, " = "))
      lines$make_next_sticky()
    }

    lines$push(elts[[i]])

    if (i < n || truncated) {
      lines$push_sticky(", ")
    }
  }

  if (truncated) {
    lines$push("...")
  }

  lines$push_sticky(">")
  lines$decrease_indent()

  lines$get_lines()
}

list_deparse <- function(x, lines = new_lines()) {
  if (!length(x) && !is_null(names(x))) {
    lines$push("<named list>")
    return(lines$get_lines())
  }

  max_elements <- lines$max_elements

  lines$push(paste0("<list: "))
  lines$increase_indent()
  truncated <- !is.null(max_elements) && length(x) > max_elements
  if (truncated) {
    x <- .subset(x, seq_len(max_elements))
  }

  nms <- deparsed_names(x)
  n <- length(x)

  for (i in seq_len(n)) {
    nm <- nms[[i]]
    if (nzchar(nm)) {
      lines$push(paste0(nm, " = "))
      lines$make_next_sticky()
    }

    lines$deparse(x[[i]])

    if (i < n || truncated) {
      lines$push_sticky(", ")
    }
  }

  if (truncated) {
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
    return(s3_deparse(x, lines))
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

needs_backticks <- function(str) {
  if (!is_string(str)) {
    str <- as_string(str)
  }

  n <- nchar(str)
  if (!n) {
    return(FALSE)
  }

  if (str %in% reserved_words) {
    return(TRUE)
  }

  start <- substr(str, 1, 1)
  if (!grepl("[[:alpha:].]", start)) {
    return(TRUE)
  }

  if (n == 1) {
    return(FALSE)
  }

  remaining <- substr(str, 2, n)

  # .0 double literals
  if (start == "." && grepl("^[[:digit:]]", remaining)) {
    return(TRUE)
  }

  grepl("[^[:alnum:]_.]", remaining)
}

# From gram.y
reserved_words <- c(
  "NULL",
  "NA",
  "TRUE",
  "FALSE",
  "Inf",
  "NaN",
  "NA_integer_",
  "NA_real_",
  "NA_character_",
  "NA_complex_",
  "function",
  "while",
  "repeat",
  "for",
  "if",
  "in",
  "else",
  "next",
  "break"
)

deparsed_names <- function(x) {
  encodeString(names2(x))
}

#' Create a default name for an R object
#'
#' @description
#'
#' `as_label()` transforms R objects into a short, human-readable
#' description. You can use labels to:
#'
#' * Display an object in a concise way, for example to labellise axes
#'   in a graphical plot.
#'
#' * Give default names to columns in a data frame. In this case,
#'   labelling is the first step before name repair.
#'
#' See also [as_name()] for transforming symbols back to a
#' string. Unlike `as_label()`, `as_name()` is a well defined
#' operation that guarantees the roundtrip symbol -> string ->
#' symbol.
#'
#' In general, if you don't know for sure what kind of object you're
#' dealing with (a call, a symbol, an unquoted constant), use
#' `as_label()` and make no assumption about the resulting string. If
#' you know you have a symbol and need the name of the object it
#' refers to, use [as_name()]. For instance, use `as_label()` with
#' objects captured with `enquo()` and `as_name()` with symbols
#' captured with `ensym()`.
#'
#' @param x An object.
#'
#' @section Transformation to string:
#'
#' * Quosures are [squashed][quo_squash] before being labelled.
#' * Symbols are transformed to string with `as_string()`.
#' * Calls are abbreviated.
#' * Numbers are represented as such.
#' * Other constants are represented by their type, such as `<dbl>`
#'   or `<data.frame>`.
#'
#' @seealso [as_name()] for transforming symbols back to a string
#'   deterministically.
#'
#' @examples
#' # as_label() is useful with quoted expressions:
#' as_label(expr(foo(bar)))
#'
#' as_label(expr(foobar))
#'
#' # It works with any R object. This is also useful for quoted
#' # arguments because the user might unquote constant objects:
#' as_label(1:3)
#'
#' as_label(base::list)
#' @export
as_label <- function(x) {
  x <- quo_squash(x)

  if (is_missing(x)) {
    return("<empty>")
  }

  switch(
    typeof(x),
    NULL = "NULL",
    symbol = as_string(x),
    language = {
      if (is_data_pronoun(x)) {
        return(data_pronoun_name(x) %||% "<unknown>")
      }
      if (infix_overflows(x)) {
        return(as_label_infix(x))
      }

      name <- deparse_one(x)
      name <- gsub("\n.*$", "...", name)
      name
    },
    if (is_bare_atomic(x, n = 1)) {
      name <- expr_text(x)
      name <- gsub("\n.*$", "...", name)
      name
    } else {
      paste0("<", rlang_type_sum(x), ">")
    }
  )
}

infix_overflows <- function(x) {
  call_print_type(x) %in% c("infix", "subset") &&
    length(expr_deparse(x, width = 60)) > 1
}
as_label_infix <- function(x) {
  # Shorten the expression if we're too long. Preserve the left side
  # if possible.
  infix_n <- nchar_infix(x)
  dots_n <- 3

  left_width <- 60 - infix_n - dots_n
  left <- expr_deparse(x[[2]], width = left_width)
  if (length(left) > 1 || nchar(left) > left_width) {
    x[[2]] <- quote(...)
    left_n <- dots_n
  } else {
    left_n <- nchar(left)
  }

  right_width <- 60 - left_n - infix_n
  right <- expr_deparse(x[[3]], width = right_width)
  if (length(right) > 1 || nchar(right) > right_width) {
    x[[3]] <- quote(...)
  }

  out <- expr_deparse(x, width = 60)

  # In case something went wrong
  if (length(out) > 1) {
    if (testing()) {
      abort("Deparsed `out` can't be multiline.", .internal = TRUE)
    }
    paste(out[[1]], "...")
  } else {
    out
  }
}
nchar_infix <- function(x) {
  x[c(2, 3)] <- 1
  nchar(expr_deparse(x)) - 2
}

#' Extract names from symbols
#'
#' @description
#'
#' `as_name()` converts [symbols][sym] to character strings. The
#' conversion is deterministic. That is, the roundtrip `symbol -> name
#' -> symbol` always gives the same result.
#'
#' - Use `as_name()` when you need to transform a symbol to a string
#'   to _refer_ to an object by its name.
#'
#' - Use [as_label()] when you need to transform any kind of object to
#'   a string to _represent_ that object with a short description.
#'
#' Expect `as_name()` to gain
#' [name-repairing](https://principles.tidyverse.org/names-attribute.html#minimal-unique-universal)
#' features in the future.
#'
#' @param x A string or symbol, possibly wrapped in a [quosure][quosure].
#'   If a string, the attributes are removed, if any.
#' @return A character vector of length 1.
#'
#' @details
#' `rlang::as_name()` is the _opposite_ of [base::as.name()]. If
#' you're writing base R code, we recommend using [base::as.symbol()]
#' which is an alias of `as.name()` that follows a more modern
#' terminology (R types instead of S modes).
#'
#' @seealso [as_label()] for converting any object to a single string
#'   suitable as a label. [as_string()] for a lower-level version that
#'   doesn't unwrap quosures.
#'
#' @examples
#' # Let's create some symbols:
#' foo <- quote(foo)
#' bar <- sym("bar")
#'
#' # as_name() converts symbols to strings:
#' foo
#' as_name(foo)
#'
#' typeof(bar)
#' typeof(as_name(bar))
#'
#' # as_name() unwraps quosured symbols automatically:
#' as_name(quo(foo))
#' @export
as_name <- function(x) {
  if (is_quosure(x)) {
    x <- quo_get_expr(x)
  }
  as_string(x)
}

call_deparse_highlight <- function(call, arg) {
  stopifnot(is_call(call))

  if (!is_string(arg)) {
    arg <- NULL
  }

  local_error_highlight()

  if (!is_call_simple(call) || call_print_fine_type(call) != "call") {
    return(format_code_unquoted(as_label(call)))
  }

  names <- names(call)
  if (!is_null(arg) && arg %in% names) {
    # Simply remove other arguments for now
    call <- call[c(1, match(arg, names))]

    args_list <- sprintf("%s = %s", arg, as_label(call[[arg]]))
    args_list <- format_arg_unquoted(args_list)
  } else {
    args_list <- args_deparse(node_cdr(call))
    args_list <- substring(args_list, 2, nchar(args_list) - 1)
  }

  head <- call[[1]]
  if (is_symbol(head)) {
    fn <- sym_text(head)
  } else {
    fn <- as_label(head)
  }

  open <- format_code_unquoted(sprintf("%s(", fn))
  close <- format_code_unquoted(")")

  paste0(open, args_list, close)
}
