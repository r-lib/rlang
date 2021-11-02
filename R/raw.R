#' Serialize a raw vector to a string
#'
#' @keywords internal
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function converts a raw vector to a hexadecimal string,
#' optionally adding a prefix and a suffix.
#' It is roughly equivalent to
#' `paste0(prefix, paste(format(x), collapse = ""), suffix)`
#' and much faster.
#'
#' @param x A raw vector.
#' @param prefix,suffix Prefix and suffix strings, or `NULL.
#'
#' @return A string.
#' @export
#' @examples
#' raw_deparse_str(raw())
#' raw_deparse_str(charToRaw("string"))
#' raw_deparse_str(raw(10), prefix = "'0x", suffix = "'")
raw_deparse_str <- function(x, prefix = NULL, suffix = NULL) {
  if (!is.null(prefix)) {
    prefix <- enc2utf8(prefix)
  }

  if (!is.null(suffix)) {
    suffix <- enc2utf8(suffix)
  }

  .Call("ffi_raw_deparse_str", x, prefix, suffix)
}
