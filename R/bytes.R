#' Human readable memory sizes
#'
#' @description
#' Construct, manipulate and display vectors of byte sizes. These are numeric
#' vectors, so you can compare them numerically, but they can also be compared
#' to human readable values such as '10MB'.
#'
#' - `parse_bytes()` takes a character vector of human-readable bytes
#'   and returns a structured bytes vector.
#'
#' - `as_bytes()` is a generic conversion function for objects
#'   representing bytes.
#'
#' Note: A `bytes()` constructor will be exported soon.
#'
#' @details
#' These memory sizes are always assumed to be base 1024, rather than 1000.
#'
#' @param x A numeric or character vector. Character representations can use
#'   shorthand sizes (see examples).
#' @examples
#' parse_bytes("1")
#' parse_bytes("1K")
#' parse_bytes("1Kb")
#' parse_bytes("1KiB")
#' parse_bytes("1MB")
#'
#' parse_bytes("1KB") < "1MB"
#'
#' sum(parse_bytes(c("1MB", "5MB", "500KB")))
#' @name bytes-class
NULL

# To be renamed to `bytes()` once next version of vctrs is on CRAN
# https://github.com/r-lib/vctrs/commit/04f1857e
bytes2 <- function(...) {
  dots <- map(list(...), ~ unclass(bytes_cast(.x)))
  new_bytes(inject(c(!!!dots)))
}


# Constructors and core methods -------------------------------------------

new_bytes <- function(x) {
  x <- x %||% dbl()
  stopifnot(is.numeric(x))
  structure(x, class = c("rlib_bytes", "numeric"))
}

bytes_cast <- function(x) {
  if (!is.object(x)) {
    switch(
      typeof(x),
      logical = if (is_unspecified(x)) return(new_bytes(as.double(x))),
      integer = ,
      double = return(new_bytes(x)),
      character = return(parse_bytes(x))
    )
  }
  abort(sprintf(
    "Can't coerce %s to <rlib_bytes>.",
    friendly_type_of(x)
  ))
}

#' @export
`[.rlib_bytes` <- function(x, i) {
  new_bytes(NextMethod("["))
}
#' @export
`[[.rlib_bytes` <- function(x, i) {
  new_bytes(NextMethod("[["))
}


# Generic conversion ------------------------------------------------------

#' @rdname bytes-class
#' @export
as_bytes <- function(x) {
  UseMethod("as_bytes")
}
#' @export
as_bytes.rlib_bytes <- function(x) {
  x
}
#' @export
as_bytes.character <- function(x) {
  parse_bytes(x)
}
#' @export
as_bytes.numeric <- function(x) {
  new_bytes(x)
}

on_package_load("methods", {
  methods::setOldClass(c("as_bytes", "numeric"), numeric())
})


# Parsing -----------------------------------------------------------------

#' @rdname bytes-class
#' @export
parse_bytes <- function(x) {
  stopifnot(is_character(x))

  pos <- regexpr(
    "^(?<size>[[:digit:].]+)\\s*(?<unit>[KMGTPEZY]?)i?[Bb]?$",
    x,
    perl = TRUE
  )
  m <- captures(x, pos)
  m$unit[m$unit == ""] <- "B"

  new_bytes(unname(as.numeric(m$size) * byte_units[m$unit]))
}

byte_units <- c(
  'B' = 1,
  'K' = 1024,
  'M' = 1024 ^ 2,
  'G' = 1024 ^ 3,
  'T' = 1024 ^ 4,
  'P' = 1024 ^ 5,
  'E' = 1024 ^ 6,
  'Z' = 1024 ^ 7,
  'Y' = 1024 ^ 8
)

captures <- function(x, m) {
  if (!is_character(x)) {
    abort("`x` must be a character.")
  }
  if (!is_reg_match(m)) {
    abort("`m` must be a match object from `regexpr()`.")
  }

  starts <- attr(m, "capture.start")
  strings <- substring(
    x,
    starts,
    starts + attr(m, "capture.length") - 1L
  )

  out <- data.frame(
    matrix(strings, ncol = NCOL(starts)),
    stringsAsFactors = FALSE
  )
  colnames(out) <- auto_name_seq(attr(m, "capture.names"))
  out[is.na(m) | m == -1, ] <- NA_character_

  out
}

is_reg_match <- function(x) {
  if (!inherits(x, "integer")) {
    return(FALSE)
  }

  nms <- c(
    "match.length",
    "capture.start",
    "capture.length",
    "capture.names"
  )
  all(nms %in% names(attributes(x)))
}

auto_name_seq <- function(names) {
  void <- detect_void_name(names)
  if (!any(void)) {
    return(names)
  }
  names[void] <- seq_along(names)[void]
  names
}


# Printing ----------------------------------------------------------------

# Adapted from https://github.com/gaborcsardi/prettyunits
#' @export
format.rlib_bytes <- function(x,
                              ...,
                              digits = 3,
                              scientific = FALSE,
                              drop0trailing = TRUE) {
  check_dots_used()

  nms <- names(x)
  bytes <- unclass(x)

  unit <- map_chr(x, find_unit, byte_units)
  res <- round(bytes / byte_units[unit], digits = digits)

  ## Zero bytes
  res[bytes == 0] <- 0
  unit[bytes == 0] <- "B"

  ## NA and NaN bytes
  res[is.na(bytes)] <- NA_real_
  res[is.nan(bytes)] <- NaN
  unit[is.na(bytes)] <- ""            # Includes NaN as well

  # Append an extra B to each unit
  large_units <- unit %in% names(byte_units)[-1]
  unit[large_units] <- paste0(unit[large_units], "B")

  res <- format(
    res,
    scientific = scientific,
    digits = digits,
    drop0trailing = drop0trailing,
    ...
  )

  stats::setNames(paste0(res, unit), nms)
}

tolerance <- sqrt(.Machine$double.eps)

find_unit <- function(x, units) {
  if (is.na(x) || is.nan(x) || x <= 0 || is.infinite(x)) {
    return(NA_character_)
  }
  epsilon <- 1 - (x * (1 / units))

  out <- which(epsilon < tolerance)
  out <- out[length(out)]
  names(out)
}

#' @export
as.character.rlib_bytes <- format.rlib_bytes

#' @export
print.rlib_bytes <- function(x, ...) {
  check_dots_used()

  # Disambiguate edge cases
  if (!length(x) || all(is.na(x))) {
    writeLines("<rlib:bytes>")
  }

  if (length(x)) {
    print(format(x, ...), quote = FALSE)
  } else {
    writeLines("[1] (empty)")
  }
}


# Arithmetic --------------------------------------------------------------

#' @export
sum.rlib_bytes <- function(x, ...) {
  new_bytes(NextMethod())
}

#' @export
min.rlib_bytes <- function(x, ...) {
  new_bytes(NextMethod())
}

#' @export
max.rlib_bytes <- function(x, ...) {
  new_bytes(NextMethod())
}

#' @export
# Adapted from Ops.numeric_version
Ops.rlib_bytes <- function (e1, e2) {
  if (nargs() == 1L) {
    abort(sprintf("unary `%s` not defined for <rlib_bytes> objects", .Generic))
  }

  boolean <- switch(
    .Generic,
    `+` = TRUE,
    `-` = TRUE,
    `*` = TRUE,
    `/` = TRUE,
    `^` = TRUE,
    `<` = TRUE,
    `>` = TRUE,
    `==` = TRUE,
    `!=` = TRUE,
    `<=` = TRUE,
    `>=` = TRUE,
    FALSE
  )
  if (!boolean) {
    abort(sprintf("`%s` not defined for <rlib_bytes> objects", .Generic))
  }

  e1 <- as_bytes(e1)
  e2 <- as_bytes(e2)
  NextMethod(.Generic)
}


# Integration -------------------------------------------------------------

# Lazily exported

pillar_shaft.rlib_bytes <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format.rlib_bytes(x), align = "right", ...)
}
type_sum.rlib_bytes <- function(x, ...) {
  "byt"
}
scale_type.rlib_bytes <- function(x) {
  "rlib_bytes"
}

on_load({
  s3_register("pillar::pillar_shaft", "rlib_bytes")
  s3_register("pillar::type_sum", "rlib_bytes")
  s3_register("ggplot2::scale_type", "rlib_bytes")
})
