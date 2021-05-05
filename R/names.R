names_as_unique <- function(names, ..., quiet = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_names_as_unique, names, quiet)
}

names_describe_repair <- function(orig_name, name) {
  if (is_null(orig_name)) {
    return()
  }

  stopifnot(length(orig_name) == length(name))

  orig_name <- orig_name %|% ""
  new_names <- name != orig_name
  if (any(new_names)) {
    bullets <- paste0(
      tick_if_needed(orig_name[new_names]),
      " -> ",
      tick_if_needed(name[new_names]),
      .problem = ""
    )
    inform(c(
      "New names:",
      set_names(bullets, "*"))
    )
  }
}

tick_if_needed <- function(x) {
  needs_ticks <- !map_lgl(x, needs_backticks)
  x[needs_ticks] <- backtick(x[needs_ticks])
  x
}
backtick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}
