
# This file serves as a reference for compatibility functions for old
# versions of R.


# R 3.2.0 ------------------------------------------------------------

if (utils::packageVersion("base") < "3.2.0") {

  dir_exists <- function(path) {
    !identical(path, "") && file.exists(paste0(path, .Platform$file.sep))
  }
  dir.exists <- function(paths) {
    vapply_lgl(paths, dir_exists)
  }

  names <- function(x) {
    if (is.environment(x)) {
      ls(x, all.names = TRUE)
    } else {
      base::names(x)
    }
  }

}
