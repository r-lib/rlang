
substitute_ <- function(x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  call <- substitute(substitute(x, env), list(x = x))
  eval_bare(call)
}

drop_last <- function(x) {
  x[-length(x)]
}
drop_first <- function(x) {
  x[-1]
}
set_names2 <- function(x, nms = names2(x)) {
  empty <- nms == ""
  nms[empty] <- x[empty]
  names(x) <- nms
  x
}

imap <- function(.x, .f, ...) {
  idx <- names(.x) %||% seq_along(.x)
  out <- Map(.f, idx, .x, ...)
  names(out) <- names(.x)
  out
}
imap_chr <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "character")
}

map_around <- function(.x, .neighbour = c("right", "left"), .f, ...) {
  where <- arg_match(.neighbour)
  n <- length(.x)
  out <- vector("list", n)

  if (n == 0) {
    return(.x)
  }

  if (n == 1) {
    out[[1]] <- .f(.x[[1]], missing_arg(), ...)
    return(out)
  }

  if (n > 1 && where == "right") {
    neighbours <- .x[seq(2, n)]
    idx <- seq_len(n - 1)
    out[idx] <- Map(.f, .x[idx], neighbours, ...)
    out[[n]] <- .f(.x[[n]], missing_arg(), ...)
    return(out)
  }

  if (n > 1 && where == "left") {
    neighbours <- .x[seq(1, n - 1)]
    idx <- seq(2, n)
    out[idx] <- Map(.f, .x[idx], neighbours, ...)
    out[[1]] <- .f(.x[[1]], missing_arg(), ...)
    return(out)
  }

  stop("unimplemented")
}

discard_unnamed <- function(x) {
  if (is_environment(x)) {
    x
  } else {
    discard(x, names2(x) == "")
  }
}

cat_line <- function(..., .trailing = TRUE, file = "") {
  cat(paste_line(..., .trailing = .trailing), file = file)
}
paste_line <- function(..., .trailing = FALSE) {
  text <- chr(...)

  if (.trailing) {
    paste0(text, "\n", collapse = "")
  } else {
    paste(text, collapse = "\n")
  }
}

has_crayon <- function() {
  is_installed("crayon") && crayon::has_color()
}

red       <- function(x) if (has_crayon()) crayon::red(x)       else x
blue      <- function(x) if (has_crayon()) crayon::blue(x)      else x
green     <- function(x) if (has_crayon()) crayon::green(x)     else x
yellow    <- function(x) if (has_crayon()) crayon::yellow(x)    else x
magenta   <- function(x) if (has_crayon()) crayon::magenta(x)   else x
cyan      <- function(x) if (has_crayon()) crayon::cyan(x)      else x
blurred   <- function(x) if (has_crayon()) crayon::blurred(x)   else x
silver    <- function(x) if (has_crayon()) crayon::silver(x)    else x
bold      <- function(x) if (has_crayon()) crayon::bold(x)      else x
italic    <- function(x) if (has_crayon()) crayon::italic(x)    else x
underline <- function(x) if (has_crayon()) crayon::underline(x) else x

open_red     <- function() if (has_crayon()) open_style("red")
open_blue    <- function() if (has_crayon()) open_style("blue")
open_green   <- function() if (has_crayon()) open_style("green")
open_yellow  <- function() if (has_crayon()) open_style("yellow")
open_magenta <- function() if (has_crayon()) open_style("magenta")
open_cyan    <- function() if (has_crayon()) open_style("cyan")
close_colour <- function() if (has_crayon()) "\u001b[39m"
close_italic <- function() if (has_crayon()) "\u001b[23m"

open_yellow_italic   <- function() if (has_crayon()) "\u001b[33m\u001b[3m"
open_blurred_italic  <- function() if (has_crayon()) "\u001b[2m\u001b[3m"
close_blurred_italic <- function() if (has_crayon()) "\u001b[23m\u001b[22m"

bullet <- function(x) {
  paste0(bold(silver("* ")), x)
}


open_style <- function(style) {
  paste0("\u001b[", codes[[style]][[1]], "m")
}
close_style <- function(style) {
  paste0("\u001b[", codes[[style]][[2]], "m")
}

ansi_regex <- paste0(
  "(?:(?:\\x{001b}\\[)|\\x{009b})",
  "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
  "|\\x{001b}[A-M]"
)
strip_style <- function(x) {
  gsub(ansi_regex, "", x, perl = TRUE)
}

codes <- list(
  reset =           c(0L, 0L),
  bold =            c(1L, 22L),
  blurred =         c(2L, 22L),
  italic =          c(3L, 23L),
  underline =       c(4L, 24L),
  inverse =         c(7L, 27L),
  hidden =          c(8L, 28L),
  strikethrough =   c(9L, 29L),

  black =           c(30L, 39L),
  red =             c(31L, 39L),
  green =           c(32L, 39L),
  yellow =          c(33L, 39L),
  blue =            c(34L, 39L),
  magenta =         c(35L, 39L),
  cyan =            c(36L, 39L),
  white =           c(37L, 39L),
  silver =          c(90L, 39L),

  bgBlack =         c(40L, 49L),
  bgRed =           c(41L, 49L),
  bgGreen =         c(42L, 49L),
  bgYellow =        c(43L, 49L),
  bgBlue =          c(44L, 49L),
  bgMagenta =       c(45L, 49L),
  bgCyan =          c(46L, 49L),
  bgWhite =         c(47L, 49L)
)


`$.r6lite` <- function(self, arg) {
  field <- env_get(self, as_string(substitute(arg)), inherit = TRUE)

  if (is_function(field)) {
    expr_interp(function(...) {
      # Unquote the method so it is printable
      method <- !!field
      method(self, ...)
    })
  } else {
    field
  }
}
r6lite <- function(...) {
  structure(new_environment(list2(...)), class = "r6lite")
}
child_r6lite <- function(.parent, ...) {
  structure(child_env(.parent, ...), class = "r6lite")
}

inc <- function(x) {
  x + 1L
}
dec <- function(x) {
  x - 1L
}

pluralise <- function(n, singular, plural) {
  if (n == 1) {
    singular
  } else {
    plural
  }
}
pluralise_along <- function(x, singular, plural) {
  pluralise(length(x), singular, plural)
}
pluralise_n <- function(n, singular, plural) {
  pluralise(n, singular, plural)
}

pad_spaces <- function(x, left = TRUE) {
  widths <- nchar(x)
  pads <- max(widths) - widths

  if (left) {
    paste0(spaces(pads), x)
  } else {
    paste0(x, spaces(pads))
  }
}

info <- function() {
  i <- if (is_installed("cli")) cli::symbol$info else "i"
  blue(i)
}
cross <- function() {
  x <- if (is_installed("cli")) cli::symbol$cross else "x"
  red(x)
}

strip_trailing_newline <- function(x) {
  n <- nchar(x)
  if (substr(x, n, n) == "\n") {
    substr(x, 0, n - 1L)
  } else {
    x
  }
}

unstructure <- function(x) {
  out <- x
  attributes(out) <- NULL

  dim(out) <- dim(x)
  names(out) <- names(x)

  out
}

cli_rule <- function() {
  if (is_installed("cli")) {
    cli::rule()
  } else {
    strrep("-", peek_option("width") %||% 60L)
  }
}

split_lines <- function(x) {
  strsplit(x, "\n", fixed = TRUE)[[1]]
}

stop_internal <- function(fn, msg) {
  abort(sprintf("Internal error in `%s()`: %s"), fn, msg)
}

with_srcref <- function(src, env = caller_env(), file = NULL) {
  file <- file %||% tempfile("sourced", fileext = ".R")
  on.exit(unlink(file))

  writeLines(src, file)
  source(file, local = env, keep.source = TRUE)
}
