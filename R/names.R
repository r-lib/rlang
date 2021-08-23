names_as_unique <- function(names, ..., quiet = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_names_as_unique, names, quiet)
}

#' Inform about name repair
#'
#' @section Muffling and silencing messages:
#'
#' Name repair messages are signaled with [inform()] and are given the class
#' `"rlib_message_name_repair"`. These messages can be muffled with
#' [base::suppressMessages()].
#'
#' Name repair messages can also be silenced with the global option
#' `rlib_name_repair_verbosity`. This option takes the values:
#'
#' - `"verbose"`: Always verbose.
#' - `"quiet"`: Always quiet.
#'
#' When set to quiet, the message is not displayed and the condition is not
#' signaled. This is particularly useful for silencing messages during testing
#' when combined with [local_options()].
#'
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

  if (peek_name_repair_verbosity() == "quiet") {
    return(invisible())
  }

  old <- old %|% ""
  new_names <- new != old

  if (!any(new_names)) {
    return(invisible())
  }

  bullets <- paste0(
    map_chr(old[new_names], format_var),
    " -> ",
    map_chr(new[new_names], format_var),
    .problem = ""
  )

  message <- c(
    "New names:",
    set_names(bullets, "*")
  )

  inform(message = message, class = "rlib_message_name_repair")
}

peek_name_repair_verbosity <- function() {
  opt <- "rlib_name_repair_verbosity"
  out <- peek_option(opt) %||% "verbose"
  out <- arg_match0(out, c("verbose", "quiet"), opt)
  out
}
