# nocov start - compat-cli.R
# Latest version: https://github.com/r-lib/rlang/blob/master/R/compat-cli.R

# Provides a minimal shim API to format message elements consistently
# with cli in packages that can't depend on it. If available, cli is
# used to format the elements. Otherwise a fallback format is used.
#
# Changelog:
#
# 2021-05-18:
#
# * Added `symbol_` and corresponding `ansi_` functions to create
#   unicode symbols if possible. The `ansi_` variants apply default
#   colours to these symbols if possible.
#
# * Added `ansi_` functions to apply ANSI styling (colours, slant, weight).
#
# * Added `format_error()` and variants to format messages with
#   cli (including bullets).
#
# * Added `cli_escape()` to escape glue and cli syntax.
#
# * `style_` functions now produce `{.cli input}` tags to be formatted
#   with one of the message formatter (such as `format_error()`). They
#   all gain a `format_` variant that formats eagerly. Eager
#   formatting is easier to work with but might produce incorrect
#   styling in very specific cases involving sophisticated cli themes.
#
#
# 2021-05-11:
#
# * Initial version.


#' Create unicode symbols
#'
#' The `symbol_` functions generate Unicode symbols if cli is
#' installed and Unicode is enabled. The corresponding `ansi_`
#' functions apply default ANSI colours to these symbols if possible.
#'
#' @noRd
symbol_info   <- function() if (requireNamespace("cli")) cli::symbol$info else "i"
symbol_cross  <- function() if (requireNamespace("cli")) cli::symbol$cross else "x"
symbol_tick   <- function() if (requireNamespace("cli")) cli::symbol$tick else "v"
symbol_bullet <- function() if (requireNamespace("cli")) cli::symbol$bullet else "*"
symbol_arrow  <- function() if (requireNamespace("cli")) cli::symbol$arrow_right else ">"
symbol_alert  <- function() "!"

ansi_info   <- function() ansi_blue(symbol_info())
ansi_cross  <- function() ansi_red(symbol_cross())
ansi_tick   <- function() ansi_green(symbol_tick())
ansi_bullet <- function() ansi_cyan(symbol_bullet())
ansi_arrow  <- function() symbol_arrow()
ansi_alert  <- function() ansi_yellow(symbol_alert())


#' Apply ANSI styling
#'
#' The `ansi_` functions style their inputs using the relevant ANSI
#' escapes if cli is installed and ANSI colours are enabled.
#'
#' @param x A string.
#'
#' @noRd
ansi_red       <- function(x) if (.rlang_cli_has_ansi()) cli::col_red(x) else x
ansi_blue      <- function(x) if (.rlang_cli_has_ansi()) cli::col_blue(x) else x
ansi_green     <- function(x) if (.rlang_cli_has_ansi()) cli::col_green(x) else x
ansi_yellow    <- function(x) if (.rlang_cli_has_ansi()) cli::col_yellow(x) else x
ansi_magenta   <- function(x) if (.rlang_cli_has_ansi()) cli::col_magenta(x) else x
ansi_cyan      <- function(x) if (.rlang_cli_has_ansi()) cli::col_cyan(x) else x
ansi_silver    <- function(x) if (.rlang_cli_has_ansi()) cli::col_silver(x) else x
ansi_blurred   <- function(x) if (.rlang_cli_has_ansi()) cli::style_blurred(x) else x
ansi_bold      <- function(x) if (.rlang_cli_has_ansi()) cli::style_bold(x) else x
ansi_italic    <- function(x) if (.rlang_cli_has_ansi()) cli::style_italic(x) else x
ansi_underline <- function(x) if (.rlang_cli_has_ansi()) cli::style_underline(x) else x

#' Apply inline styling
#'
#' @description
#' This set of `style_` and `format_` functions create consistent
#' inline styling, using cli if available or an ASCII fallback style
#' otherwise.
#'
#' * The `style_` functions create actual `{.span input}` tags when
#'   cli is available. The resulting strings must then be formatted
#'   using `format_error()` or a variant. These message formatters
#'   then process the cli style tags.
#'
#' * The `format_` functions are easier to work with because they
#'   format the style eagerly. However they produce slightly incorrect
#'   style in corner cases because the formatting doesn't take into
#'   account the message type. In principle, cli themes can create
#'   different stylings depending on the message type.
#'
#' @param x A string.
#'
#' @noRd
style_emph   <- function(x) .rlang_cli_style_inline(x, "emph", "_%s_")
style_strong <- function(x) .rlang_cli_style_inline(x, "strong", "*%s*")
style_code   <- function(x) .rlang_cli_style_inline(x, "code", "`%s`")
style_q      <- function(x) .rlang_cli_style_inline(x, "q", NULL)
style_pkg    <- function(x) .rlang_cli_style_inline(x, "pkg", NULL)
style_fn     <- function(x) .rlang_cli_style_inline(x, "fn", "`%s()`")
style_arg    <- function(x) .rlang_cli_style_inline(x, "arg", "`%s`")
style_kbd    <- function(x) .rlang_cli_style_inline(x, "kbd", "[%s]")
style_key    <- function(x) .rlang_cli_style_inline(x, "key", "[%s]")
style_file   <- function(x) .rlang_cli_style_inline(x, "file", NULL)
style_path   <- function(x) .rlang_cli_style_inline(x, "path", NULL)
style_email  <- function(x) .rlang_cli_style_inline(x, "email", NULL)
style_url    <- function(x) .rlang_cli_style_inline(x, "url", "<%s>")
style_var    <- function(x) .rlang_cli_style_inline(x, "var", "`%s`")
style_envvar <- function(x) .rlang_cli_style_inline(x, "envvar", "`%s`")
style_field  <- function(x) .rlang_cli_style_inline(x, "field", NULL)

style_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_style_inline(x, "cls", fallback)
}

format_emph   <- function(x) .rlang_cli_format_inline(x, "emph", "_%s_")
format_strong <- function(x) .rlang_cli_format_inline(x, "strong", "*%s*")
format_code   <- function(x) .rlang_cli_format_inline(x, "code", "`%s`")
format_q      <- function(x) .rlang_cli_format_inline(x, "q", NULL)
format_pkg    <- function(x) .rlang_cli_format_inline(x, "pkg", NULL)
format_fn     <- function(x) .rlang_cli_format_inline(x, "fn", "`%s()`")
format_arg    <- function(x) .rlang_cli_format_inline(x, "arg", "`%s`")
format_kbd    <- function(x) .rlang_cli_format_inline(x, "kbd", "[%s]")
format_key    <- function(x) .rlang_cli_format_inline(x, "key", "[%s]")
format_file   <- function(x) .rlang_cli_format_inline(x, "file", NULL)
format_path   <- function(x) .rlang_cli_format_inline(x, "path", NULL)
format_email  <- function(x) .rlang_cli_format_inline(x, "email", NULL)
format_url    <- function(x) .rlang_cli_format_inline(x, "url", "<%s>")
format_var    <- function(x) .rlang_cli_format_inline(x, "var", "`%s`")
format_envvar <- function(x) .rlang_cli_format_inline(x, "envvar", "`%s`")
format_field  <- function(x) .rlang_cli_format_inline(x, "field", NULL)

format_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_format_inline(x, "cls", fallback)
}

.rlang_cli_style_inline <- function(x, span, fallback = "`%s`") {
  if (.rlang_cli_has_ansi()) {
    paste0("{.", span, " {\"", encodeString(x), "\"}}")
  } else if (is.null(fallback)) {
    x
  } else if (is.function(fallback)) {
    fallback(x)
  } else {
    sprintf(fallback, x)
  }
}
.rlang_cli_format_inline <- function(x, span, fallback = "`%s`") {
  if (.rlang_cli_has_ansi()) {
    cli::format_message(paste0("{.", span, " {x}}"))
  } else {
    .rlang_cli_style_inline(x, span, fallback = fallback)
  }
}

#' Format messages
#'
#' @description
#'
#' These format functions use cli if available to format condition
#' messages. This includes structural formatting:
#'
#' - Styling as a function of the message type (error, warning,
#'   message).
#' - Bullets formatting (info, alert, ...).
#' - Indented width wrapping.
#'
#' This also applies inline formatting in combination with the
#' `style_` prefixed functions.
#'
#' The input should not contain any `"{foo}"` glue syntax. If you are
#' assembling a message from multiple pieces, use `cli_escape()` on
#' user or external inputs that might contain curly braces.
#'
#' @param x A character vector of lines. Names define bullet types.
#'
#' @noRd
format_error <- function(x) {
  .rlang_cli_format(x, cli::format_error)
}
#' @rdname format_error
#' @noRd
format_warning <- function(x) {
  .rlang_cli_format(x, cli::format_warning)
}
#' @rdname format_error
#' @noRd
format_message <- function(x) {
  .rlang_cli_format(x, cli::format_message)
}

.rlang_cli_format <- function(x, cli_format) {
  if (.rlang_cli_has_ansi()) {
    out <- cli_format(x, .envir = emptyenv())
    out <- .rlang_cli_str_restore(out, x)
    return(out)
  }

  if (!length(x)) {
    return(x)
  }

  nms <- names(x)

  if (is_null(nms)) {
    nms <- rep_len("", length(x))
  }

  if (requireNamespace("rlang")) {
    abort <- rlang::abort
  } else {
    abort <- function(message) stop(message, call. = FALSE)
  }
  if (!all(nms %in% c("i", "x", "v", "*", "!", ">", " ", ""))) {
    abort('Bullet names must be one of "i", "x", "v", "*", "!", ">", or " ".')
  }

  bullets <-
    ifelse(nms == "i", ansi_info(),
    ifelse(nms == "x", ansi_cross(),
    ifelse(nms == "v", ansi_tick(),
    ifelse(nms == "*", ansi_bullet(),
    ifelse(nms == "!", ansi_alert(),
    ifelse(nms == ">", ansi_arrow(),
    ifelse(nms == "", "",
    ifelse(nms == " ", " ",
      "*"))))))))

  bullets <-
    ifelse(bullets == "", "", paste0(bullets, " "))

  paste0(bullets, x, collapse = "\n")
}

.rlang_cli_str_restore <- function(x, to) {
  to <- to[1]
  to[[1]] <- x
  to
}

.rlang_cli_has_ansi <- function() {
  requireNamespace("cli") && cli::num_ansi_colors() > 1
}

#' Escape cli and glue syntax
#'
#' This doubles all `{` and `}` characters to prevent them from being
#' interpreted as syntax for glue interpolation or cli styling.
#'
#' @param x A character vector.
#'
#' @noRd
cli_escape <- function(x) {
  gsub("\\}", "}}", gsub("\\{", "{{", x))
}


# nocov end
