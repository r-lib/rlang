#' Signal a deprecated or obsolete function
#'
#' @description
#'
#' These functions signal a function as deprecated or obsolete.
#'
#' * A deprecated function does not issue a warning unless the
#'   `rlang_verbose_deprecation` option is set to `TRUE`. This is
#'   often the first stage towards obsolescence. It is recommended to
#'   stop using deprecated functions and switch to suggested
#'   replacements.
#'
#' * An obsolete function always issues a warning when called. Users
#'   should stop using that function as it might get removed in the
#'   future.
#'
#' @param name The name of the depreciated function as a string.
#' @param replacement The name of the suggested replacement as a
#'   string.
#' @param start The package version where the function was
#'   depreciated.
#' @export
#' @examples
#' # A deprecated function is signalled silently:
#' foo <- function() signal_deprecated("foo", "bar", "0.2.0")
#' foo()
#'
#' # If the `rlang_verbose_deprecation` option is TRUE, a warning is
#' # issued. If run, the following would throw a warning:
#' #> options(rlang_verbose_deprecation = TRUE)
#' #> foo()
signal_deprecated <- function(name, replacement, start = NULL) {
  if (is_true(peek_option("rlang_verbose_deprecation"))) {
    signal <- cnd_warn
  } else {
    signal <- cnd_signal
  }

  msg <- deprecation_msg("deprecated", name, replacement, start)

  signal("deprecated",
    replacement = replacement,
    start = start,
    .msg = msg
  )
}
#' @rdname signal_deprecated
#' @export
warn_obsolete <- function(name, replacement, start = NULL) {
  msg <- deprecation_msg("obsolete", name, replacement, start)

  cnd_warn("obsolete",
    replacement = replacement,
    start = start,
    .msg = msg
  )
}

deprecation_msg <- function(type, name, replacement, start = NULL) {
  stopifnot(
    is_string(name),
    is_string(replacement)
  )

  msg <- sprintf("`%s` is %s", name, type)
  if (!is_null(start)) {
    msg <- sprintf("%s as of version %s", msg, start)
  }

  paste0(msg, sprintf(", please use `%s` instead", replacement))
}







}

new_cycle <- function(cycle) {
  if (!length(cycle) || length(cycle) > 3) {
    abort("`cycle` must have 1, 2, or 3 components")
  }
  if (is_character(cycle)) {
    cycle <- chr_as_cycle(cycle)
  }

  cycle_check(cycle, n_components = 3, max_digits = 2, minor = FALSE)

  cycle
}
chr_as_cycle <- function(cycle) {
  if (all(cycle == "")) {
    abort("`cycle` can't be empty")
  }

  # Replace empty versions with "0.0.0" as this simplifies code later
  # on, e.g. for comparison
  cycle[cycle == ""] <- "0.0.0"

  cycle <- map(cycle, ver)

  # All cycles must have 3 components
  if (length(cycle) < 3) {
    n <- length(cycle)
    n_missing <- 3 - n

    filler <- list_len(n_missing)
    cycle <- c(cycle, filler)

    for (i in seq_len(n_missing)) {
      cycle[[n + i]] <- ver_bump(cycle[[n + i - 1]], "minor")
    }
  }

  cycle
}

cycle_check <- function(cycle, n_components, max_digits, minor) {
  is_empty <- map_lgl(cycle, identical, ver("0.0.0"))
  trimmed_cycle <- cycle[!is_empty]
  map(trimmed_cycle, ver_check, n_components, max_digits, minor)

  if (length(trimmed_cycle) > 1) {
    if (any(slide_lgl(trimmed_cycle, `>=`))) {
      abort("`cycle` versions must be monotonically increasing")
    }
  }
}
ver_check <- function(ver, n_components = NULL, max_digits = NULL, minor = NULL) {
  if (!is_version(ver)) {
    abort("can't parse version")
  }

  components <- ver_components(ver)

  if (!is_version(ver, n_components = n_components)) {
    msg <- "version must have %s components, not %s"
    msg <- sprintf(msg, n_components, length(components))
    abort(msg)
  }

  if (!is_version(ver, max_digits = max_digits)) {
    msg <- "version can't have components with more than %s digits"
    msg <- sprintf(msg, max_digits)
    abort(msg)
  }

  if (!is_version(ver, minor = minor)) {
    if (minor) {
      abort("version must be a minor update")
    } else {
      abort("version can't be a minor update")
    }
  }

  invisible(TRUE)
}

ver <- function(x) {
  stopifnot(is_string(x))
  as_version(as.numeric_version(x))
}
new_version <- function(x) {
  stopifnot(is_integerish(x))
  ver(paste(x, collapse = "."))
}
as_version <- function(x) {
  if (inherits(x, "numeric_version")) {
    set_attrs(x, class = "version")
  } else {
    abort("Can't convert `x` to a version")
  }
}

is_version <- function(x, n_components = NULL, max_digits = NULL, minor = NULL) {
  if (!inherits(x, "version")) {
    return(FALSE)
  }

  components <- ver_components(x)

  if (!is_null(n_components) && length(components) != n_components) {
    return(FALSE)
  }

  if (!is_null(max_digits)) {
    large <- log10(components) >= max_digits
    if (any(large)) {
      return(FALSE)
    }
  }

  if (!is_null(minor)) {
    is_minor <-  components[[length(components)]] != 0
    if (!identical(minor, is_minor)) {
      return(FALSE)
    }
  }

  TRUE
}

ver_components <- function(ver) {
  flatten_int(ver)
}

ver_bump <- function(ver, component = c("patch", "minor", "major")) {
  stopifnot(is_version(ver, n_components = 3))

  i <- switch(component,
    patch = 1L,
    minor = 2L,
    major = 3L
  )

  components <- ver_components(ver)
  components[[i]] <- components[[i]] + 1L


`[[.version` <- function(x, i) {
  ver_components(x)[[i]]
}
`[[<-.version` <- function(x, i, value) {
  components <- ver_components(x)
  components[[i]] <- value
  new_version(components)
}
length.version <- function(x) {
  length(ver_components(x))
}
Ops.version <- function(e1, e2) {
  # Ops.numeric_version() assumes the length method is not implemented
  e1 <- as.numeric_version(e1)
  e2 <- as.numeric_version(e2)

  # For some reason NextMethod() throws an error
  eval_bare(lang(.Generic, e1, e2), base_env())
}

print.version <- function(x, ...) {
  print(as.numeric_version(x))
}
as.character.version <- function(x) {
  as.character(as.numeric_version(x))
}
