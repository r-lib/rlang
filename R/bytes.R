#' Human readable memory sizes
#'
#' Construct, manipulate and display vectors of byte sizes. These are numeric
#' vectors, so you can compare them numerically, but they can also be compared
#' to human readable values such as '10MB'.
#'
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
as_bytes.numeric <- function(x) {
  new_bytes(x)
}

#' @importFrom methods setOldClass
setOldClass(c("as_bytes", "numeric"), numeric())

#' @rdname bytes-class
#' @export
parse_bytes <- function(x) {
  stopifnot(is_character(x))
  m <- captures(x, regexpr("^(?<size>[[:digit:].]+)\\s*(?<unit>[KMGTPEZY]?)i?[Bb]?$", x, perl = TRUE))
  m$unit[m$unit == ""] <- "B"
  new_bytes(unname(as.numeric(m$size) * byte_units[m$unit]))
}


new_bytes <- function(x) {
  structure(x, class = c("rlib_bytes", "numeric"))
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

# Adapted from https://github.com/gaborcsardi/prettyunits
#' @export
format.rlib_bytes <- function(x, scientific = FALSE, digits = 3, drop0trailing = TRUE, ...) {
  nms <- names(x)

  bytes <- unclass(x)

  unit <- vcapply(x, find_unit, byte_units)
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

  res <- format(res, scientific = scientific, digits = digits, drop0trailing = drop0trailing, ...)

  stats::setNames(paste0(res, unit), nms)
}

#' @export
as.character.rlib_bytes <- format.rlib_bytes

#' @export
print.rlib_bytes <- function(x, ...) {
  print(format.rlib_bytes(x, ...), quote = FALSE)
}

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
`[.rlib_bytes` <- function(x, i) {
  new_bytes(NextMethod("["))
}

#' @export
`[[.rlib_bytes` <- function(x, i) {
  new_bytes(NextMethod("[["))
}

#' @export
# Adapted from Ops.numeric_version
Ops.rlib_bytes <- function (e1, e2) {
  if (nargs() == 1L) {
    stop(sprintf("unary '%s' not defined for \"rlib_bytes\" objects", .Generic),
      call. = FALSE)
  }

  boolean <- switch(.Generic,
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
  FALSE)
  if (!boolean) {
    stop(sprintf("'%s' not defined for \"rlib_bytes\" objects", .Generic),
      call. = FALSE)
  }
  e1 <- as_bytes(e1)
  e2 <- as_bytes(e2)
  NextMethod(.Generic)
}

# Lazily exported
pillar_shaft.rlib_bytes <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format.rlib_bytes(x), align = "right", ...)
}

# Lazily exported
type_sum.rlib_bytes <- function(x) {
  "byt"
}
