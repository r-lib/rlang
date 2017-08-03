# nocov start - compat-oldrel (last updated: rlang 0.1.2)

# This file serves as a reference for compatibility functions for old
# versions of R. Please find the most recent version in rlang's
# repository.


# R 3.2.0 ------------------------------------------------------------

if (getRversion() < "3.2.0") {

  dir_exists <- function(path) {
    !identical(path, "") && file.exists(paste0(path, .Platform$file.sep))
  }
  dir.exists <- function(paths) {
    vapply(paths, dir_exists, logical(1))
  }

  names <- function(x) {
    if (is.environment(x)) {
      return(ls(x, all.names = TRUE))
    } else {
      return(base::names(x))
    }

    # So R CMD check on old versions of R sees a generic, since we
    # declare a names() method for dictionary objects
    UseMethod("names")
  }

  trimws <- function(x, which = c("both", "left", "right")) {
    switch(match.arg(which),
      left = sub("^[ \t\r\n]+", "", x, perl = TRUE),
      right = sub("[ \t\r\n]+$", "", x, perl = TRUE),
      both = trimws(trimws(x, "left"), "right")
    )
  }

}

# nocov end
