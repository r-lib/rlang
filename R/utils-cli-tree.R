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
  full_emphasis <- every(data$node_type, is_string, "main")

  labels <- data$call_text
  src_locs <- chr(map_if(data$src_loc, nzchar, ~ paste0(" at ", .x)))
  labels <- paste0(labels, style_locs(src_locs))

  res <- character()

  pt <- function(root, n = integer(), mx = integer(), deemphasise = FALSE) {
    num_root <- match(root, data$id)
    main_sibling <- is_string(data$node_type[[num_root]], "main_sibling")
    marked_deemph <- FALSE

    prefix <- map_chr(seq_along(n), function(i) {
      mark_deemph <- function(x) {
        if (!marked_deemph) {
          marked_deemph <<- TRUE
          paste0("<DEEMPH>", x)
        } else {
          x
        }
      }

      if (n[i] < mx[i]) {
        if (i == length(n)) {
          if (deemphasise) {
            mark_deemph(paste0(style$j, style$h))
          } else if (!main_sibling && !full_emphasis) {
            paste0(style$j, mark_deemph(style$h))
          } else {
            paste0(style$j, style$h)
          }
        } else {
          # Detect first "|" branch displayed by taking into account
          # the empty " " spaces
          past <- seq_len(i)
          n_spaces <- sum(n[past] >= mx[past] & past != length(n))

          if (!deemphasise || i == 1 + n_spaces) {
            paste0(style$v, " ")
          } else {
            mark_deemph(paste0(style$v, " "))
          }
        }
      } else if (n[i] == mx[i] && i == length(n)) {
        if (deemphasise) {
          mark_deemph(paste0(style$l, style$h))
        } else {
          paste0(style$l, style$h)
        }
      } else {
        "  "
      }
    })

    line <- paste0(paste(prefix, collapse = ""), labels[[num_root]])

    if (marked_deemph) {
      parts <- strsplit(line, "<DEEMPH>")[[1]]
      line <- paste0(parts[[1]], trace_deemph(parts[[2]]))
    }

    res <<- c(res, line)

    children <- data$children[[num_root]]
    deemphasise <- deemphasise || !is_string(data$node_type[[num_root]], "main")

    for (d in seq_along(children)) {
      pt(children[[d]], c(n, d), c(mx, length(children)), deemphasise = deemphasise)
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

    res <- paste0(col_silver(indices), res)
  }

  res
}

trace_deemph <- function(x) {
  deemph <- peek_option("rlang:::trace_deemph") %||% style_dim_soft
  deemph(x)
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
