# This is mostly a copy of https://github.com/r-lib/fs/blob/0f5b6191935fe4c862d2e5003655e6c1669f4afd/R/fs_bytes.R
# If I end up needing this in a third package it should probably live in a package somewhere, maybe prettyunits?

byte_units <- c('B' = 1, 'K' = 1024, 'M' = 1024 ^ 2, 'G' = 1024 ^ 3, 'T' = 1024 ^ 4, 'P' = 1024 ^ 5, 'E' = 1024 ^ 6, 'Z' = 1024 ^ 7, 'Y' = 1024 ^ 8)

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
#' bench_bytes("1")
#' bench_bytes("1K")
#' bench_bytes("1Kb")
#' bench_bytes("1KiB")
#' bench_bytes("1MB")
#'
#' bench_bytes("1KB") < "1MB"
#'
#' sum(bench_bytes(c("1MB", "5MB", "500KB")))
#' @name bench_bytes
#' @export
as_bench_bytes <- function(x) {
  UseMethod("as_bench_bytes")
}

#' @export
#' @rdname bench_bytes
bench_bytes <- as_bench_bytes

new_bench_bytes <- function(x) {
  structure(x, class = c("bench_bytes", "numeric"))
}

#' @importFrom methods setOldClass
setOldClass(c("bench_bytes", "numeric"), numeric())

#' @export
as_bench_bytes.default <- function(x) {
  x <- as.character(x)
  m <- captures(x, regexpr("^(?<size>[[:digit:].]+)\\s*(?<unit>[KMGTPEZY]?)i?[Bb]?$", x, perl = TRUE))
  m$unit[m$unit == ""] <- "B"
  new_bench_bytes(unname(as.numeric(m$size) * byte_units[m$unit]))
}

#' @export
as_bench_bytes.bench_bytes <- function(x) {
  return(x)
}

#' @export
as_bench_bytes.numeric <- function(x) {
  new_bench_bytes(x)
}

# Adapted from https://github.com/gaborcsardi/prettyunits
#' @export
format.bench_bytes <- function(x, scientific = FALSE, digits = 3, drop0trailing = TRUE, ...) {
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
as.character.bench_bytes <- format.bench_bytes

#' @export
print.bench_bytes <- function(x, ...) {
  print(format.bench_bytes(x, ...), quote = FALSE)
}

#' @export
sum.bench_bytes <- function(x, ...) {
  new_bench_bytes(NextMethod())
}

#' @export
min.bench_bytes <- function(x, ...) {
  new_bench_bytes(NextMethod())
}

#' @export
max.bench_bytes <- function(x, ...) {
  new_bench_bytes(NextMethod())
}

#' @export
`[.bench_bytes` <- function(x, i) {
  new_bench_bytes(NextMethod("["))
}

#' @export
`[[.bench_bytes` <- function(x, i) {
  new_bench_bytes(NextMethod("[["))
}

#' @export
# Adapted from Ops.numeric_version
Ops.bench_bytes <- function (e1, e2) {
  if (nargs() == 1L) {
    stop(sprintf("unary '%s' not defined for \"bench_bytes\" objects", .Generic),
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
    stop(sprintf("'%s' not defined for \"bench_bytes\" objects", .Generic),
      call. = FALSE)
  }
  e1 <- as_bench_bytes(e1)
  e2 <- as_bench_bytes(e2)
  NextMethod(.Generic)
}

pillar_shaft.bench_bytes <- function(x, ...) {
  pillar::new_pillar_shaft_simple(format.bench_bytes(x), align = "right", ...)
}

type_sum.bench_bytes <- function(x) {
  "bch:byt"
}
