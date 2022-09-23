# nocov start - compat-cli.R
# Latest version: https://github.com/r-lib/rlang/blob/main/R/compat-cli.R

# Provides a minimal shim API to format message elements consistently
# with cli in packages that can't depend on it. If available, cli is
# used to format the elements. Otherwise a fallback format is used.
#
# Changelog:
#
# 2022-09-23:
#
# * `format_` functions now use `cli::format_inline()` instead of
#   `cli::format_message()`, resulting in simpler ANSI codes.
#
# * Added `format_run()` and `format_href()`.
#
#
# 2022-08-16:
#
# * Added `has_ansi()`. This checks that cli is installed and that
#   `cli::num_ansi_colors()` is greater than 1.
#
# * `col_` and `style_` functions now consistently return bare strings.
#
#
# 2022-05-23:
#
# * Added compat for `style_hyperlink()`.
#
#
# 2022-02-23:
#
# * Bullet formatting now ignores unknown bullet names, consistently
#   with cli. This increases resiliency against hard-to-detect errors
#   and improves forward compatibility.
#
#
# 2022-02-22:
#
# * `format_error()` and variants now call cli even when ANSI colours
#   are disabled.
#
# * The fallback formatting for `.emph` and `.strong` no longer
#   surrounds in `_` or `*` characters. This is consistent with cli
#   formatting.
#
#
# 2021-07-06:
#
# * Added missing `col_`, `bg_`, and `style_` functions.
#
#
# 2021-05-18:
#
# * Added `symbol_` and corresponding `ansi_` functions to create
#   unicode symbols if possible. The `ansi_` variants apply default
#   colours to these symbols if possible.
#
# * Added `style_` functions to apply ANSI styling (colours, slant, weight).
#
# * Added `format_error()` and variants to format messages with
#   cli (including bullets).
#
# * Added `cli_escape()` to escape glue and cli syntax.
#
# * `mark_` functions now produce `{.cli input}` tags to be formatted
#   with one of the message formatter (such as `format_error()`). They
#   all have a `format_` variant that formats eagerly. Eager
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
symbol_info   <- function() if (.rlang_cli_has_cli()) cli::symbol$info else "i"
symbol_cross  <- function() if (.rlang_cli_has_cli()) cli::symbol$cross else "x"
symbol_tick   <- function() if (.rlang_cli_has_cli()) cli::symbol$tick else "v"
symbol_bullet <- function() if (.rlang_cli_has_cli()) cli::symbol$bullet else "*"
symbol_arrow  <- function() if (.rlang_cli_has_cli()) cli::symbol$arrow_right else ">"
symbol_alert  <- function() "!"

ansi_info   <- function() col_blue(symbol_info())
ansi_cross  <- function() col_red(symbol_cross())
ansi_tick   <- function() col_green(symbol_tick())
ansi_bullet <- function() col_cyan(symbol_bullet())
ansi_arrow  <- function() symbol_arrow()
ansi_alert  <- function() col_yellow(symbol_alert())


#' Apply ANSI styling
#'
#' The `col_`, `bg_`, and `style_` functions style their inputs using
#' the relevant ANSI escapes if cli is installed and ANSI colours are
#' enabled.
#'
#' @param x A string.
#'
#' @noRd
col_black              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_black(x)) else x
col_blue               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_blue(x)) else x
col_cyan               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_cyan(x)) else x
col_green              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_green(x)) else x
col_magenta            <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_magenta(x)) else x
col_red                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_red(x)) else x
col_white              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_white(x)) else x
col_yellow             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_yellow(x)) else x
col_grey               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_grey(x)) else x
col_silver             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_silver(x)) else x
col_none               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::col_none(x)) else x

bg_black               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_black(x)) else x
bg_blue                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_blue(x)) else x
bg_cyan                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_cyan(x)) else x
bg_green               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_green(x)) else x
bg_magenta             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_magenta(x)) else x
bg_red                 <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_red(x)) else x
bg_white               <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_white(x)) else x
bg_yellow              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_yellow(x)) else x
bg_none                <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::bg_none(x)) else x

style_dim              <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_dim(x)) else x
style_blurred          <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_blurred(x)) else x
style_bold             <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_bold(x)) else x
style_hidden           <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_hidden(x)) else x
style_inverse          <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_inverse(x)) else x
style_italic           <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_italic(x)) else x
style_strikethrough    <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_strikethrough(x)) else x
style_underline        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_underline(x)) else x

style_no_dim           <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_dim(x)) else x
style_no_blurred       <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_blurred(x)) else x
style_no_bold          <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_bold(x)) else x
style_no_hidden        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_hidden(x)) else x
style_no_inverse       <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_inverse(x)) else x
style_no_italic        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_italic(x)) else x
style_no_strikethrough <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_strikethrough(x)) else x
style_no_underline     <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_underline(x)) else x

style_reset            <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_reset(x)) else x
style_no_colour        <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_color(x)) else x
style_no_bg_colour     <- function(x) if (.rlang_cli_has_cli()) .rlang_cli_unstructure(cli::style_no_bg_color(x)) else x

CLI_SUPPORT_HYPERLINK <- "2.2.0"
CLI_SUPPORT_HYPERLINK_PARAMS <- "3.1.1"

style_hyperlink <- function(text, url, params = NULL) {
  if (is.null(params)) {
    if (.rlang_cli_has_cli(CLI_SUPPORT_HYPERLINK)) {
      cli::style_hyperlink(text, url)
    } else {
      text
    }
  } else {
    if (.rlang_cli_has_cli(CLI_SUPPORT_HYPERLINK_PARAMS)) {
      cli::style_hyperlink(text, url, params = params)
    } else {
      text
    }
  }
}

#' Apply inline styling
#'
#' @description
#' This set of `mark_` and `format_` functions create consistent
#' inline styling, using cli if available or an ASCII fallback style
#' otherwise.
#'
#' * The `mark_` functions wrap the input with mark up tags when cli
#'   is available. For instance, `"foo"` is transformed to `{.span
#'   {\"foo\"}}`. These marked up strings must eventually be formatted
#'   using a formatter such as `format_error()` to be styled
#'   appropriately.
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
mark_emph   <- function(x) .rlang_cli_style_inline(x, "emph", "_%s_")
mark_strong <- function(x) .rlang_cli_style_inline(x, "strong", "*%s*")
mark_code   <- function(x) .rlang_cli_style_inline(x, "code", "`%s`")
mark_q      <- function(x) .rlang_cli_style_inline(x, "q", NULL)
mark_pkg    <- function(x) .rlang_cli_style_inline(x, "pkg", NULL)
mark_fn     <- function(x) .rlang_cli_style_inline(x, "fn", "`%s()`")
mark_arg    <- function(x) .rlang_cli_style_inline(x, "arg", "`%s`")
mark_kbd    <- function(x) .rlang_cli_style_inline(x, "kbd", "[%s]")
mark_key    <- function(x) .rlang_cli_style_inline(x, "key", "[%s]")
mark_file   <- function(x) .rlang_cli_style_inline(x, "file", NULL)
mark_path   <- function(x) .rlang_cli_style_inline(x, "path", NULL)
mark_email  <- function(x) .rlang_cli_style_inline(x, "email", NULL)
mark_url    <- function(x) .rlang_cli_style_inline(x, "url", "<%s>")
mark_var    <- function(x) .rlang_cli_style_inline(x, "var", "`%s`")
mark_envvar <- function(x) .rlang_cli_style_inline(x, "envvar", "`%s`")
mark_field  <- function(x) .rlang_cli_style_inline(x, "field", NULL)

mark_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_style_inline(x, "cls", fallback)
}

format_emph   <- function(x) .rlang_cli_format_inline(x, "emph", "%s")
format_strong <- function(x) .rlang_cli_format_inline(x, "strong", "%s")
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
format_href   <- function(x, target = NULL) .rlang_cli_format_inline_link(x, target, "href", "<%s>")
format_run    <- function(x, target = NULL) .rlang_cli_format_inline_link(x, target, "run", "`%s`")

format_error_arg_highlight <- function(x, quote = TRUE) {
  if (is_true(peek_option("rlang:::trace_test_highlight"))) {
    return(paste0("<<ARG ", x, ">>"))
  }
  out <- if (quote) format_arg(x) else x
  style_bold(cli::col_br_magenta(out))
}
format_error_call_highlight <- function(x, quote = TRUE) {
  if (is_true(peek_option("rlang:::trace_test_highlight"))) {
    return(paste0("<<CALL ", x, ">>"))
  }
  out <- if (quote) format_code(x) else x
  style_bold(cli::col_br_blue(out))
}

format_cls <- function(x) {
  fallback <- function(x) sprintf("<%s>", paste0(x, collapse = "/"))
  .rlang_cli_format_inline(x, "cls", fallback)
}

.rlang_cli_style_inline <- function(x, span, fallback = "`%s`") {
  if (.rlang_cli_has_cli()) {
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
  if (.rlang_cli_has_cli()) {
    cli::format_inline(paste0("{.", span, " {x}}"))
  } else {
    .rlang_cli_style_inline(x, span, fallback = fallback)
  }
}

.rlang_cli_format_inline_link <- function(x, target, span, fallback = "`%s`") {
  if (.rlang_cli_has_cli()) {
    if (is_null(target)) {
      cli::format_inline(paste0("{.", span, " {x}}"))
    } else {
      cli::format_inline(paste0("{.", span, " [{x}]({target})}"))
    }
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
  if (.rlang_cli_has_cli()) {
    out <- cli_format(x, .envir = emptyenv())
    .rlang_cli_str_restore(out, unname(x))
  } else {
    .rlang_cli_format_fallback(x)
  }
}
.rlang_cli_format_fallback <- function(x) {
  if (!length(x)) {
    return(unname(x))
  }

  nms <- names(x)

  if (is_null(nms)) {
    nms <- rep_len("", length(x))
  }

  abort <- .rlang_cli_compat("abort")

  bullets <- local({
    unicode_opt <- getOption("cli.condition_unicode_bullets")
    if (identical(unicode_opt, FALSE)) {
      old <- options(cli.unicode = FALSE)
      on.exit(options(old))
    }

    # For consistency with `cli::format_error()` and for resiliency
    # against hard-to-detect errors (see #1364), unknown names are
    # silently ignored. This also makes it easier to add new bullet
    # names in the future with forward-compatibility.
    ifelse(nms == "i", ansi_info(),
    ifelse(nms == "x", ansi_cross(),
    ifelse(nms == "v", ansi_tick(),
    ifelse(nms == "*", ansi_bullet(),
    ifelse(nms == "!", ansi_alert(),
    ifelse(nms == ">", ansi_arrow(),
    ifelse(nms == "", "",
    ifelse(nms == " ", " ",
      ""))))))))
  })

  bullets <-
    ifelse(bullets == "", "", paste0(bullets, " "))

  out <- paste0(bullets, x, collapse = "\n")
  .rlang_cli_str_restore(out, unname(x))
}

.rlang_cli_str_restore <- function(x, to) {
  out <- to

  out <- out[1]
  out[[1]] <- x

  # Restore attributes only if unclassed. It is assumed the `[` and
  # `[[` methods deal with attributes in case of classed objects.
  # Preserving attributes matters for the assertthat package for
  # instance.
  if (!is.object(to)) {
    attrib <- attributes(to)

    attrib$names <- NULL
    attrib$dim <- NULL
    attrib$dimnames <- NULL
    attrib <- c(attributes(out), attrib)

    attributes(out) <- attrib
  }

  out
}

has_ansi <- function() {
  .rlang_cli_has_cli() && cli::num_ansi_colors() > 1
}

.rlang_cli_has_cli <- local({
  cache <- new.env()

  function(version = "3.0.0") {
    out <- cache[[version]]

    if (is.null(out)) {
      out <- cache[[version]] <<-
        requireNamespace("cli", quietly = TRUE) &&
        utils::packageVersion("cli") >= version
    }

    out
  }
})

#' Escape cli and glue syntax
#'
#' This doubles all `{` and `}` characters to prevent them from being
#' interpreted as syntax for glue interpolation or cli styling.
#'
#' @param x A character vector.
#'
#' @noRd
cli_escape <- function(x) {
  if (.rlang_cli_has_cli()) {
    gsub("\\}", "}}", gsub("\\{", "{{", x))
  } else {
    x
  }
}


.rlang_cli_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    # Make sure rlang knows about "x" and "i" bullets
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}

.rlang_cli_unstructure <- function(x) {
  attributes(x) <- NULL
  x
}

# nocov end
