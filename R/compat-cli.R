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
# * Added `ansi_` functions to apply ANSI styling (colours, slant, weight).
#
#
# 2021-05-11:
#
# * Initial version.


#' Apply ANSI styling
#'
#' The `ansi_` functions style their inputs using the relevant ANSI
#' escapes if cli is installed and ANSI colours are enabled.
#'
#' @param x A string.
#'
#' @noRd
ansi_red       <- function(x) if (.rlang_cli_has_cli()) cli::col_red(x) else x
ansi_blue      <- function(x) if (.rlang_cli_has_cli()) cli::col_blue(x) else x
ansi_green     <- function(x) if (.rlang_cli_has_cli()) cli::col_green(x) else x
ansi_yellow    <- function(x) if (.rlang_cli_has_cli()) cli::col_yellow(x) else x
ansi_magenta   <- function(x) if (.rlang_cli_has_cli()) cli::col_magenta(x) else x
ansi_cyan      <- function(x) if (.rlang_cli_has_cli()) cli::col_cyan(x) else x
ansi_silver    <- function(x) if (.rlang_cli_has_cli()) cli::col_silver(x) else x
ansi_blurred   <- function(x) if (.rlang_cli_has_cli()) cli::style_blurred(x) else x
ansi_bold      <- function(x) if (.rlang_cli_has_cli()) cli::style_bold(x) else x
ansi_italic    <- function(x) if (.rlang_cli_has_cli()) cli::style_italic(x) else x
ansi_underline <- function(x) if (.rlang_cli_has_cli()) cli::style_underline(x) else x

style_emph   <- function(x) .rlang_cli_style(x, "emph", "_%s_")
style_strong <- function(x) .rlang_cli_style(x, "strong", "*%s*")
style_code   <- function(x) .rlang_cli_style(x, "code", "`%s`")
style_q      <- function(x) .rlang_cli_style(x, "q", NULL)
style_pkg    <- function(x) .rlang_cli_style(x, "pkg", NULL)
style_fn     <- function(x) .rlang_cli_style(x, "fn", "`%s()`")
style_arg    <- function(x) .rlang_cli_style(x, "arg", "`%s`")
style_kbd    <- function(x) .rlang_cli_style(x, "kbd", "[%s]")
style_key    <- function(x) .rlang_cli_style(x, "key", "[%s]")
style_file   <- function(x) .rlang_cli_style(x, "file", NULL)
style_path   <- function(x) .rlang_cli_style(x, "path", NULL)
style_email  <- function(x) .rlang_cli_style(x, "email", NULL)
style_url    <- function(x) .rlang_cli_style(x, "url", "<%s>")
style_var    <- function(x) .rlang_cli_style(x, "var", "`%s`")
style_envvar <- function(x) .rlang_cli_style(x, "envvar", "`%s`")
style_field  <- function(x) .rlang_cli_style(x, "field", NULL)

style_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_style(x, "cls", fallback)
}

.rlang_cli_style <- function(x, span, fallback = "`%s`") {
  if (requireNamespace("cli", quietly = TRUE) && cli::num_ansi_colors() > 1) {
    cli::format_message(paste0("{.", span, " {x}}"))
  } else if (is.null(fallback)) {
    x
  } else if (is.function(fallback)) {
    fallback(x)
  } else {
    sprintf(fallback, x)
  }
}

.rlang_cli_has_cli <- function() {
  requireNamespace("cli") && cli::num_ansi_colors() > 1
}

# nocov end
