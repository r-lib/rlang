#' Coerce to a character vector and attempt encoding conversion
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' Unlike specifying the `encoding` argument in `as_string()` and
#' `as_character()`, which is only declarative, these functions
#' actually attempt to convert the encoding of their input. There are
#' two possible cases:
#'
#' * The string is tagged as UTF-8 or latin1, the only two encodings
#'   for which R has specific support. In this case, converting to the
#'   same encoding is a no-op, and converting to native always works
#'   as expected, as long as the native encoding, the one specified by
#'   the `LC_CTYPE` locale has support for all characters occurring in
#'   the strings. Unrepresentable characters are serialised as unicode
#'   points: "<U+xxxx>".
#'
#' * The string is not tagged. R assumes that it is encoded in the
#'   native encoding. Conversion to native is a no-op, and conversion
#'   to UTF-8 should work as long as the string is actually encoded in
#'   the locale codeset.
#'
#' When translating to UTF-8, the strings are parsed for serialised
#' unicode points (e.g. strings looking like "U+xxxx") with
#' [chr_unserialise_unicode()]. This helps to alleviate the effects of
#' character-to-symbol-to-character roundtrips on systems with
#' non-UTF-8 native encoding.
#'
#' @param x An object to coerce.
#' @export
#' @examples
#' # Let's create a string marked as UTF-8 (which is guaranteed by the
#' # Unicode escaping in the string):
#' utf8 <- "caf\uE9"
#' Encoding(utf8)
#' as_bytes(utf8)
as_utf8_character <- function(x) {
  .Call(rlang_unescape_character, as_character(x))
}

#' Translate unicode points to UTF-8
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' For historical reasons, R translates strings to the native encoding
#' when they are converted to symbols. This string-to-symbol
#' conversion is not a rare occurrence and happens for instance to the
#' names of a list of arguments converted to a call by `do.call()`.
#'
#' If the string contains unicode characters that cannot be
#' represented in the native encoding, R serialises those as an ASCII
#' sequence representing the unicode point. This is why Windows users
#' with western locales often see strings looking like `<U+xxxx>`. To
#' alleviate some of the pain, rlang parses strings and looks for
#' serialised unicode points to translate them back to the proper
#' UTF-8 representation. This transformation occurs automatically in
#' functions like [env_names()] and can be manually triggered with
#' `as_utf8_character()` and `chr_unserialise_unicode()`.
#'
#'
#' @section Life cycle:
#'
#' This function is experimental.
#'
#' @param chr A character vector.
#' @export
#' @keywords internal
#' @examples
#' ascii <- "<U+5E78>"
#' chr_unserialise_unicode(ascii)
#'
#' identical(chr_unserialise_unicode(ascii), "\u5e78")
chr_unserialise_unicode <- function(chr) {
  stopifnot(is_character(chr))
  .Call(rlang_unescape_character, chr)
}

#' Create a string
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' These base-type constructors allow more control over the creation
#' of strings in R. They take character vectors or string-like objects
#' (integerish or raw vectors), and optionally set the encoding. The
#' string version checks that the input contains a scalar string.
#'
#' @param x A character vector or a vector or list of string-like
#'   objects.
#' @param encoding If non-null, set an encoding mark. This is only
#'   declarative, no encoding conversion is performed.
#' @export
#' @examples
#' # As everywhere in R, you can specify a string with Unicode
#' # escapes. The characters corresponding to Unicode codepoints will
#' # be encoded in UTF-8, and the string will be marked as UTF-8
#' # automatically:
#' cafe <- string("caf\uE9")
#' Encoding(cafe)
#' as_bytes(cafe)
#'
#' # In addition, string() provides useful conversions to let
#' # programmers control how the string is represented in memory. For
#' # encodings other than UTF-8, you'll need to supply the bytes in
#' # hexadecimal form. If it is a latin1 encoding, you can mark the
#' # string explicitly:
#' cafe_latin1 <- string(c(0x63, 0x61, 0x66, 0xE9), "latin1")
#' Encoding(cafe_latin1)
#' as_bytes(cafe_latin1)
string <- function(x, encoding = NULL) {
  if (is_integerish(x)) {
    x <- rawToChar(as.raw(x))
  } else if (is_raw(x)) {
    x <- rawToChar(x)
  } else if (!is_string(x)) {
    abort("`x` must be a string or raw vector")
  }

  if (!is_null(encoding)) {
    Encoding(x) <- encoding
  }

  x
}

#' Coerce to a raw vector
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' This currently only works with strings, and returns its hexadecimal
#' representation.
#'
#'
#' @section Life cycle:
#'
#' Raw vector functions are experimental.
#'
#' @param x A string.
#' @return A raw vector of bytes.
#' @keywords internal
#' @export
as_bytes <- function(x) {
  switch(typeof(x),
    raw = return(x),
    character = if (is_string(x)) return(charToRaw(x))
  )
  abort("`x` must be a string or raw vector")
}
new_bytes <- function(x) {
  if (is_integerish(x)) {
    as.raw(x)
  } else if (is_raw(x)) {
    x
  } else {
    abort("input should be integerish")
  }
}
