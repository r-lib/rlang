names_as_unique <- function(names, ..., quiet = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_names_as_unique, names, quiet)
}

#' Inform about names repair
#' @param old Original names vector.
#' @param new Repaired names vector.
#' @keywords internal
#' @export
names_inform_repair <- function(old, new) {
  if (is_null(old)) {
    old <- rep_along(new, "")
  }
  stopifnot(
    is_character(old),
    is_character(new),
    length(old) == length(new)
  )

  old <- old %|% ""
  new_names <- new != old
  if (any(new_names)) {
    bullets <- paste0(
      tick_if_needed(old[new_names]),
      " -> ",
      tick_if_needed(new[new_names]),
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
