# nocov start - compat-rlang.R
# Latest version: https://github.com/r-lib/rlang/blob/main/R/compat-rlang.R

# Changelog:
#
# 2022-09-16:
# * No longer uses rlang if not fully loaded (#1482)
#
# 2020-05-26:
# * Initial version.


# These versions of `abort()`, `warn()` and `inform()` are only
# guaranteed to support "i" and "x" bullets. Other kinds of bullets
# might fail if rlang is not recent enough.


.rlang_compat <- function(fn, try_rlang = TRUE) {
  # Compats that behave the same independently of rlang's presence
  out <- switch(
    fn,
    is_installed = return(function(pkg) requireNamespace(pkg, quietly = TRUE))
  )

  # Only use rlang if it is fully loaded (#1482)
  if (try_rlang &&
        requireNamespace("rlang", quietly = TRUE) &&
        environmentIsLocked(asNamespace("rlang"))) {
    switch(
      fn,
      is_interactive = return(rlang::is_interactive)
    )

    # Make sure rlang knows about "x" and "i" bullets
    if (utils::packageVersion("rlang") >= "0.4.2") {
      switch(
        fn,
        abort = return(rlang::abort),
        warn = return((rlang::warn)),
        inform = return(rlang::inform)
      )
    }
  }

  # Fall back to base compats

  is_interactive_compat <- function() {
    opt <- getOption("rlang_interactive")
    if (!is.null(opt)) {
      opt
    } else {
      interactive()
    }
  }

  format_msg <- function(x) paste(x, collapse = "\n")
  switch(
    fn,
    is_interactive = return(is_interactive_compat),
    abort = return(function(msg) stop(format_msg(msg), call. = FALSE)),
    warn = return(function(msg) warning(format_msg(msg), call. = FALSE)),
    inform = return(function(msg) message(format_msg(msg)))
  )

  stop(sprintf("Internal error in rlang shims: Unknown function `%s()`.", fn))
}


#nocov end
