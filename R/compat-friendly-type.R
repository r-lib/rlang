# nocov start --- r-lib/rlang compat-friendly-type
#
# Changelog
# =========
#
# 2021-12-20:
# - Added support for scalar values and empty vectors.
# - Added `stop_input_type()`
#
# 2021-06-30:
# - Added support for missing arguments.
#
# 2021-04-19:
# - Added support for matrices and arrays (#141).
# - Added documentation.
# - Added changelog.


#' Return English-friendly type
#' @param x Any R object.
#' @param value Whether to describe the value of `x`.
#' @param length Whether to mention the length of vectors and lists.
#' @return A string describing the type. Starts with an indefinite
#'   article, e.g. "an integer vector".
#' @noRd
friendly_type_of <- function(x, value = TRUE, length = FALSE) {
  if (is_missing(x)) {
    return("absent")
  }

  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    } else {
      type <- paste(class(x), collapse = "/")
    }
    return(sprintf("a <%s> object", type))
  }

  if (!rlang::is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }

  n_dim <- length(dim(x))

  if (value && !n_dim) {
    if (is_na(x)) {
      return(switch(
        typeof(x),
        logical = "`NA`",
        integer = "an integer `NA`",
        double = "a numeric `NA`",
        complex = "a complex `NA`",
        character = "a character `NA`",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
    if (length(x) == 1 && !is_list(x)) {
      return(switch(
        typeof(x),
        logical = if (x) "`TRUE`" else "`FALSE`",
        integer = "an integer",
        double = "a number",
        complex = "a complex number",
        character = if (nzchar(x)) "a string" else "`\"\"`",
        raw = "a raw value",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
    if (length(x) == 0) {
      return(switch(
        typeof(x),
        logical = "an empty logical vector",
        integer = "an empty integer vector",
        double = "an empty numeric vector",
        complex = "an empty complex vector",
        character = "an empty character vector",
        raw = "an empty raw vector",
        list = "an empty list",
        .rlang_stop_unexpected_typeof(x)
      ))
    }
  }

  type <- .rlang_as_friendly_vector_type(typeof(x), n_dim)

  if (length && !n_dim) {
    type <- paste0(type, sprintf(" of length %s", length(x)))
  }

  type
}

.rlang_as_friendly_vector_type <- function(type, n_dim) {
  if (type == "list") {
    if (n_dim < 2) {
      return("a list")
    } else if (n_dim == 2) {
      return("a list matrix")
    } else {
      return("a list array")
    }
  }

  type <- switch(
    type,
    logical = "a logical %s",
    integer = "an integer %s",
    numeric = ,
    double = "a double %s",
    complex = "a complex %s",
    character = "a character %s",
    raw = "a raw %s",
    type = paste0("a ", type, " %s")
  )

  if (n_dim < 2) {
    kind <- "vector"
  } else if (n_dim == 2) {
    kind <- "matrix"
  } else {
    kind <- "array"
  }
  sprintf(type, kind)
}

.rlang_as_friendly_type <- function(type) {
  switch(
    type,

    list = "a list",

    NULL = "NULL",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}

.rlang_stop_unexpected_typeof <- function(x, call = rlang::caller_env()) {
  rlang::abort(
    sprintf("Unexpected type <%s>.", typeof(x)),
    call = call
  )
}

#' @param x The object type which does not conform to `what`. Its
#'   `friendly_type_of()` is taken and mentioned in the error message.
#' @param what The friendly expected type.
#' @param ... Arguments passed to [abort()].
#' @inheritParams args_error_context
#' @noRd
stop_input_type <- function(x,
                            what,
                            ...,
                            arg = rlang::caller_arg(x),
                            call = rlang::caller_env()) {
  # From compat-cli.R
  format_arg <- rlang::env_get(
    nm = "format_arg",
    last = topenv(),
    default = NULL
  )
  if (!is.function(format_arg)) {
    format_arg <- function(x) sprintf("`%s`", x)
  }

  message <- sprintf(
    "%s must be %s, not %s.",
    format_arg(arg),
    what,
    friendly_type_of(x)
  )
  rlang::abort(message, ..., call = call)
}

# nocov end
