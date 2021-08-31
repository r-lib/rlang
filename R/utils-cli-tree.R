# Extract from cli::tree
# Modifications:
# * Remove assertions (requires assertthat)
# * Remove width triming (requires ansistrings or similar)
# * Use map_chr() instead of vcapply()
# Additional functions inlined:
# * is_utf8_output()
# * is_latex_output()

cli_tree <- function(data,
                     root = data$id[[1]],
                     style = NULL) {
  style <- style %||% cli_box_chars()

  labels <- data$call_text
  src_locs <- chr(map_if(data$src_loc, nzchar, ~ paste0(" at ", .x)))
  labels <- paste0(labels, style_locs(src_locs))

  res <- character()

  pt <- function(root, n = integer(), mx = integer()) {

    num_root <- match(root, data$id)

    level <- length(n) - 1
    prefix <- map_chr(seq_along(n), function(i) {
      if (n[i] < mx[i]) {
        if (i == length(n)) {
          paste0(style$j, style$h)
        } else {
          paste0(style$v, " ")
        }
      } else if (n[i] == mx[i] && i == length(n)) {
        paste0(style$l, style$h)
      } else {
        "  "
      }
    })

    res <<- c(res, paste0(paste(prefix, collapse = ""), labels[[num_root]]))

    children <- data$children[[num_root]]
    for (d in seq_along(children)) {
      pt(children[[d]], c(n, d), c(mx, length(children)))
    }
  }

  if (nrow(data)) {
    pt(root)
  }

  indices <- data$id[-1]

  if (length(indices)) {
    indices <- pad_spaces(indices)
    indices <- paste0(" ", indices, ". ")

    # The root isn't numbered
    root_padding <- spaces(nchar(indices[[1]]))
    indices <- c(root_padding, indices)

    res <- paste0(silver(indices), res)
  }

  res
}

cli_box_chars <- function() {
  if (cli_is_utf8_output()) {
    list(
      "h" = "\u2500",                   # horizontal
      "v" = "\u2502",                   # vertical
      "l" = "\u2514",                   # leaf
      "j" = "\u251C"                    # junction
    )
  } else {
    list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",                       # leaf
      "j" = "+"                         # junction
    )
  }
}

cli_is_utf8_output <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !cli_is_latex_output()
  }
}

cli_is_latex_output <- function() {
  if (!("knitr" %in% loadedNamespaces())) return(FALSE)
  get("is_latex_output", asNamespace("knitr"))()
}
