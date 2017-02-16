#' Construct a character vector or a string.
#'
#' These base-type constructors allow more control over the creation
#' of strings in R. They take character vectors or string-like objects
#' (integerish or raw vectors), and optionally set the encoding. The
#' string version checks that the input contains a scalar string.
#'
#' @param x A character vector or a vector or list of string-like
#'   objects.
#' @param encoding If non-null, passed to
#'   \code{\link{chr_set_encoding}()} to add an encoding mark.
#' @seealso \code{\link{chr_set_encoding}()} for more information
#'   about encodings in R.
#' @export
#' @examples
#' # As everywhere in R, you can specify a string with Unicode
#' # escapes. The characters corresponding to Unicode codepoints will
#' # be encoded in UTF-8, and the string will be marked as UTF-8
#' # automatically:
#' cafe <- string("caf\uE9")
#' str_encoding(cafe)
#' str_bytes(cafe)
#'
#' # In addition, string() provides useful conversions to let
#' # programmers control how the string is represented in memory. For
#' # encodings other than UTF-8, you'll need to supply the bytes in
#' # hexadecimal form. If it is a latin1 encoding, you can mark the
#' # string explicitly:
#' cafe_latin1 <- string(c(0x63, 0x61, 0x66, 0xE9), "latin1")
#' str_encoding(cafe_latin1)
#' str_bytes(cafe_latin1)
#'
#' # chr() accepts lists and will apply string() to each element:
#' chr(list(cafe, c(0x63, 0x61, 0x66, 0xE9)))
string <- function(x, encoding = NULL) {
  if (is_integerish(x)) {
    x <- rawToChar(as.raw(x))
  } else if (is_raw(x)) {
    x <- rawToChar(x)
  } else if (!is_string(x)) {
    abort("`x` must be a string or raw vector")
  }

  chr_set_encoding(x, encoding)
}
#' @rdname string
#' @export
chr <- function(x, encoding = NULL) {
  if (!is_character(x)) {
    x <- map_chr(x, string)
  }
  chr_set_encoding(x, encoding)
}

#' Coerce to a character vector.
#'
#' The input is coerced to a string character vector, bypassing method
#' dispatch. \code{as_character()} and \code{string()} have an
#' optional \code{encoding} argument to specify the encoding. R uses
#' this information for internal handling of strings and character
#' vectors. Note that this is only declarative, no encoding conversion
#' is attempted. See \code{\link{as_utf8_character}()} and
#' \code{\link{as_native_character}()} for coercing to a character
#' vector and attempt encoding conversion.
#'
#' @seealso \code{\link{chr_set_encoding}()} and
#'   \code{\link{set_utf8_locale}()} for information about encodings
#'   and locales in R, and \code{\link{string}()} and
#'   \code{\link{chr}()} for other ways of creating strings and
#'   character vectors.
#' @inheritParams string
#' @param x An object to coerce.
#' @export
#' @examples
#' # Let's create a string marked as UTF-8 (which is guaranteed by the
#' # Unicode escaping in the string):
#' utf8 <- "caf\uE9"
#' str_encoding(utf8)
#' str_bytes(utf8)
#'
#' # It can then be converted to a native encoding, that is, the
#' # encoding specified in the current locale:
#' \dontrun{
#' set_latin1_locale()
#' latin1 <- as_native_string(utf8)
#' str_encoding(latin1)
#' str_bytes(latin1)
#' }
as_character <- function(x, encoding = NULL) {
  x <- as.character(unclass(x))
  chr_set_encoding(x, encoding)
}
#' @rdname as_character
#' @export
as_utf8_character <- function(x) {
  enc2utf8(as_character(x))
}
#' @rdname as_character
#' @export
as_native_character <- function(x) {
  enc2native(as_character(x))
}
#' @rdname as_character
#' @export
as_string <- function(x, encoding = NULL) {
  x <- as_character(x, encoding)
  stopifnot(is_string(x))
  x
}
#' @rdname as_character
#' @export
as_utf8_string <- function(x) {
  x <- as_utf8_character(x)
  stopifnot(is_string(x))
  x
}
#' @rdname as_character
#' @export
as_native_string <- function(x) {
  x <- as_native_character(x)
  stopifnot(is_string(x))
  x
}

#' Set encoding of a string or character vector.
#'
#' R has specific support for UTF-8 and latin1 encoded strings. This
#' mostly matters for internal conversions. Thanks to this support,
#' you can reencode strings to UTF-8 or latin1 for internal
#' processing, and return these strings without having to convert them
#' back to the native encoding. However, it is important to make sure
#' the encoding mark has not been lost in the process, otherwise the
#' output will be treated as if encoded according to the current
#' locale (see \code{\link{set_utf8_locale}()} for documentation about
#' locale codesets), which is not appropriate if it does not coincide
#' with the actual encoding. In those situations, you can use these
#' functions to ensure an encoding mark in your strings.
#'
#' @param x A string or character vector.
#' @param encoding Either an encoding specially handled by R
#'   (\code{"UTF-8"} or \code{"latin1"}), \code{"bytes"} to inhibit
#'   all encoding conversions, or \code{"unknown"} if the string
#'   should be treated as encoded in the current locale codeset.
#' @seealso \code{\link{set_utf8_locale}()} about the effects of the
#'   locale, and \code{\link{as_utf8_string}()} about encoding
#'   conversion.
#' @export
#' @examples
#' # Encoding marks are always ignored on ASCII strings:
#' str_encoding(str_set_encoding("cafe", "UTF-8"))
#'
#' # You can specify the encoding of strings containing non-ASCII
#' # characters:
#' cafe <- string(c(0x63, 0x61, 0x66, 0xC3, 0xE9))
#' str_encoding(cafe)
#' str_encoding(str_set_encoding(cafe, "UTF-8"))
#'
#'
#' # It is important to consistently mark the encoding of strings
#' # because R and other packages perform internal string conversions
#' # all the time. Here is an example with the names attribute:
#' latin1 <- string(c(0x63, 0x61, 0x66, 0xE9), "latin1")
#' latin1 <- set_names(latin1)
#'
#' # The names attribute is encoded in latin1 as we would expect:
#' str_encoding(names(latin1))
#'
#' # However the names are converted to UTF-8 by the c() function:
#' str_encoding(names(c(latin1)))
#' str_bytes(names(c(latin1)))
#'
#' # Bad things happen when the encoding marker is lost and R performs
#' # a conversion. R will assume that the string is encoded according
#' # to the current locale:
#' \dontrun{
#' bad <- set_names(str_set_encoding(latin1, "unknown"))
#' set_utf8_locale()
#'
#' str_encoding(names(c(bad)))
#' str_bytes(names(c(bad)))
#' }
chr_set_encoding <- function(x, encoding = c("unknown", "UTF-8", "latin1", "bytes")) {
  if (!is_null(encoding)) {
    Encoding(x) <- match.arg(encoding)
  }
  x
}
#' @rdname chr_set_encoding
#' @export
chr_encoding <- function(x) {
  Encoding(x)
}
#' @rdname chr_set_encoding
#' @export
str_set_encoding <- function(x, encoding = c("unknown", "UTF-8", "latin1", "bytes")) {
  stopifnot(is_string(x))
  chr_set_encoding(x, encoding)
}
#' @rdname chr_set_encoding
#' @export
str_encoding <- function(x) {
  stopifnot(is_string(x))
  Encoding(x)
}

#' Set the locale's codeset for testing.
#'
#' Setting a locale's codeset (specifically, the \code{LC_CTYPE}
#' category) produces side effects in R's handling of strings. The
#' most important of these affects how the R parser marks strings. R
#' has specific internal support for latin1 (single-byte encoding) and
#' UTF-8 (multi-bytes variable-width encoding) strings. If the locale
#' codeset is latin1 or UTF-8, the parser will mark all strings with
#' the corresponding encoding. It is important for strings to have
#' consistent encoding markers, as they determine a number of internal
#' encoding conversions when R or packages handle strings (see
#' \code{\link{str_set_encoding}()} for some examples).
#'
#' UTF-8 is the only variable-width encoding supported in R.
#' Multi-byte fixed-width encodings like UCS-2 are supported by R, but
#' supplying UTF-16 strings (the default Unicode encoding on Windows)
#' will cause the parser to choke on the character delimiters (though
#' you may not see that behaviour in Windows IDEs if they translate
#' inputs to another encoding for you). Note finally that
#' \code{set_utf8_locale()} will not work on Windows as only latin1
#' and MBCS locales are supported on this OS.
#'
#' Note that these helpers are only provided for testing interactively
#' the effects of changing locale codeset. They let you quickly change
#' the default text encoding to latin1, UTF-8, or non-UTF-8 MBCS. They
#' are not widely tested and do not provide a way of setting the
#' language and region of the locale. They have permanent side effects
#' and should not be used in package examples, unit tests, or in the
#' course of a data analysis.
#'
#' @export
set_utf8_locale <- function() {
  if (.Platform$OS.type == "windows") {
    warn("UTF-8 is not supported on Windows")
  } else {
    inform("Locale codeset is now UTF-8")
    invisible(Sys.setlocale("LC_CTYPE", locale = "en_US.UTF-8"))
  }
}
#' @rdname set_utf8_locale
#' @export
set_latin1_locale <- function() {
  if (.Platform$OS.type == "windows") {
    locale <- "English_United States.1252"
  } else {
    locale <- "en_US.ISO8859-1"
  }
  inform("Locale codeset is now latin1")
  invisible(Sys.setlocale("LC_CTYPE", locale = locale))
}
#' @rdname set_utf8_locale
#' @export
set_mbcs_locale <- function() {
  if (.Platform$OS.type == "windows") {
    locale <- "English_United States.932"
  } else {
    locale <- "ja_JP.SJIS"
  }
  inform("Locale codeset is now of non-UTF-8 MBCS type")
  invisible(Sys.setlocale("LC_CTYPE", locale = locale))
}

#' Return a string as a raw vector.
#'
#' These helpers return the hexadecimal representation of a string.
#'
#' @param x A string or character vector.
#' @return A raw vector for \code{str_bytes()} or a list of raw
#'   vectors for \code{chr_bytes()}.
#' @export
str_bytes <- function(x) {
  stopifnot(is_string(x))
  charToRaw(x)
}
#' @rdname str_bytes
#' @export
chr_bytes <- function(x) {
  map(x, charToRaw)
}
