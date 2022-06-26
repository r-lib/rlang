deprecated <- function() missing_arg()

abort_coercion <- function(x,
                           to_type,
                           x_type = NULL,
                           arg = NULL,
                           call = caller_env()) {
  x_type <- x_type %||% obj_type_friendly(x, value = TRUE)

  if (is_null(arg)) {
    msg <- sprintf("Can't convert %s to %s.", x_type, to_type)
  } else {
    arg <- format_arg(arg)
    msg <- sprintf("Can't convert %s, %s, to %s.", arg, x_type, to_type)
  }

  abort(msg, call = call)
}

set_names2 <- function(x, nms = names2(x)) {
  empty <- nms == ""
  nms[empty] <- x[empty]
  names(x) <- nms
  x
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
open_bold    <- function() if (has_crayon()) open_style("bold")
close_colour <- function() if (has_crayon()) "\u001b[39m"
close_italic <- function() if (has_crayon()) "\u001b[23m"
close_bold   <- function() if (has_crayon()) close_style("bold")

open_yellow_italic   <- function() if (has_crayon()) "\u001b[33m\u001b[3m"
open_blurred_italic  <- function() if (has_crayon()) "\u001b[2m\u001b[3m"
close_blurred_italic <- function() if (has_crayon()) "\u001b[23m\u001b[22m"


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

# Import symbols from cli if available
on_load({
  has_cli <- is_installed("cli")
  has_cli_format <- is_installed("cli", version = "3.0.0")
  has_cli_start_app <- is_installed("cli", version = "2.0.0")
})

info <- function() {
  i <- if (has_cli) cli::symbol$info else "i"
  blue(i)
}
cross <- function() {
  x <- if (has_cli) cli::symbol$cross else "x"
  red(x)
}
tick <- function() {
  x <- if (has_cli) cli::symbol$tick else "v"
  green(x)
}
bullet <- function() {
  x <- if (has_cli) cli::symbol$bullet else "*"

  # Use small bullet if cli is too old.
  # See https://github.com/r-lib/cli/issues/241
  if (!has_cli_format && !is_string(x, "*")) {
    x <- "\u2022"
  }

  cyan(x)
}
arrow_right <- function() {
  if (has_cli) cli::symbol$arrow_right else ">"
}

style_dim_soft <- function(x) {
  if (cli::num_ansi_colors() >= 256) {
    crayon::make_style(grDevices::grey(0.6), colors =  256)(x)
  } else {
    silver(x)
  }
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

stop_internal <- function(message, ..., call = caller_env(2)) {
  abort(message, ..., call = call, .internal = TRUE)
}

stop_internal_c_lib <- function(file, line, call, message, frame) {
  if (nzchar(file)) {
    message <- c(
      message,
      "i" = sprintf(
        "In file %s at line %d.",
        format_file(file),
        line
      ))
  }
  if (!is_installed("winch") && is_interactive()) {
    message <- c(
      message,
      "i" = sprintf(
        "Install the %s package to get additional debugging info the next time you get this error.",
        format_pkg("winch")
      )
    )
  }

  abort(message, call = call, .internal = TRUE, .frame = frame)
}

with_srcref <- function(src, env = caller_env(), file = NULL) {
  file <- file %||% tempfile("sourced", fileext = ".R")
  on.exit(unlink(file))

  writeLines(src, file)
  source(file, local = env, keep.source = TRUE)
}

chr_has_curly <- function(x) {
  .Call(ffi_chr_has_curly, x)
}


new_stack <- function() {
  stack <- new_dyn_vector("list", 100)

  push <- function(...) {
    for (obj in list2(...)) {
      dyn_push_back(stack, maybe_missing(obj))
    }
  }

  # Can be used as a coro generator
  pop <- function() {
    if (dyn_count(stack)) {
      dyn_pop_back(stack)
    } else {
      exhausted()
    }
  }

  new_environment(list(
    push = push,
    pop = pop
  ))
}

exhausted <- function() as.symbol(".__exhausted__.")
is_exhausted <- function(x) identical(x, exhausted())

path_trim_prefix <- function(path, n) {
  split <- strsplit(path, "/")[[1]]
  n_split <- length(split)

  if (n_split <= n) {
    path
  } else {
    paste(split[seq2(n_split - n + 1, n_split)], collapse = "/")
  }
}

browser <- function(...,
                    skipCalls = 0,
                    frame = parent.frame()) {
  if (!identical(stdout(), getConnection(1))) {
    sink(getConnection(1))
    withr::defer(sink(), envir = frame)
  }

  # Calling `browser()` on exit avoids RStudio displaying the
  # `browser2()` location. We still need one `n` to get to the
  # expected place. Ideally `skipCalls` would not skip but exit the
  # contexts.
  on.exit(base::browser(..., skipCalls = skipCalls + 1))
}

df_print <- function(x, ...) {
  class(x) <- c("tbl", "data.frame")
  print(x, ...)
  invisible(x)
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

glue_escape <- function(x) {
  gsub("\\}", "}}", gsub("\\{", "{{", x))
}

detect_run_starts <- function(x) {
  if (!length(x)) {
    return(lgl())
  }

  lagged <- c(NA, x[-length(x)])
  x != lagged | (is.na(lagged) & !is.na(x))
}

# No ANSI support
capitalise <- function(x) {
  stopifnot(length(x) == 1)
  n <- nchar(x)
  if (n == 0) {
    x
  } else if (n == 1) {
    toupper(x)
  } else {
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  }
}

testing <- function() {
  nzchar(Sys.getenv("TESTTHAT"))
}

cli_with_whiteline_escapes <- function(x, fn) {
  x <- gsub("\n", "__NEW_LINE__", x, fixed = TRUE)
  x <- gsub(" ", "__SPACE__", x, fixed = TRUE)
  x <- fn(x)
  x <- gsub("__SPACE__", " ", x, fixed = TRUE)
  x <- gsub("__NEW_LINE__", "\n", x, fixed = TRUE)
  x
}

style_rlang_run <- function(code) {
  style_hyperlink(
    paste0("rlang::", code),
    paste0("rstudio:run:rlang::", code)
  )
}

vec_remove <- function(x, values) {
  loc <- match(values, x, nomatch = 0)
  if (sum(loc) == 0) {
    x
  } else {
    x[-loc]
  }
}

str_nzchar <- function(x) {
  is_string(x) && nzchar(x)
}
