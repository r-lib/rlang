with_non_utf8_encoding <- function(code) {
  old_encoding <- set_non_utf8_encoding()
  on.exit(set_encoding(old_encoding), add = TRUE)
  code
}

set_non_utf8_encoding <- function() {
  if (.Platform$OS.type == "windows") return(NULL)
  tryCatch(
    locale <- set_encoding("en_US.ISO88591"),
    warning = function(e) {
      tryCatch(
        locale <<- set_encoding("fr_CH.ISO8859-15"),
        warning = function(w) {
          testthat::skip("Cannot set latin-1 encoding")
        }
      )
    }
  )
  locale
}

set_encoding <- function(encoding) {
  if (is.null(encoding)) return(NULL)
  locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", encoding)
  locale
}
