#' Import `compat-*.R` files
#'
#' @description
#' `use_compat()` downloads compat files from GitHub for use in your own
#' package. For example, `use_compat("r-lib/rlang", "purrr")` places the
#' `compat-purrr.R` file from rlang into your package's `R/` directory.
#'
#' These files are typically useful when you need to minimize dependencies,
#' but still want to use a style consistent with the rest of the tidyverse.
#'
#' This function requires `repo_spec` to have a `main` branch. This is where the
#' source code is downloaded from.
#'
#' @param repo_spec A GitHub repo specified as `"owner/repo"`.
#'
#' @param what The compat file to download.
#'
#' @param overwrite Should the compat file be overwritten if it exists? If this
#'   is `FALSE` and the file exists, then an error will be thrown.
#'
#' @noRd
use_compat <- function(repo_spec, what, overwrite = FALSE) {
  check_installed(c("fs", "usethis", "glue"), "to utilize compat files.")

  if (!is_string(what)) {
    abort("`what` must be a string.")
  }

  spec <- parse_repo_spec(repo_spec)

  file_name <- glue::glue("compat-{what}.R")

  path_proj <- usethis::proj_path()

  path_repo <- download_package(spec$owner, spec$repo)
  on.exit(fs::dir_delete(path_repo), add = TRUE)
  usethis::ui_done("Downloaded source code for {usethis::ui_value(repo_spec)}.")

  path_proj_file <- fs::path(path_proj, "R", file_name)
  path_repo_file <- fs::path(path_repo, "R", file_name)

  if (!fs::file_exists(path_repo_file)) {
    abort(glue::glue(
      "Compat file named {usethis::ui_path(file_name)} doesn't exist in",
      "{usethis::ui_value(repo_spec)}.",
      .sep = " "
    ))
  }

  fs::file_copy(
    path = path_repo_file,
    new_path = path_proj_file,
    overwrite = overwrite
  )

  usethis::ui_done(glue::glue(
    "Copied {usethis::ui_path(file_name)} from",
    "{usethis::ui_value(repo_spec)} to",
    "{usethis::ui_path(path_proj_file)}.",
    .sep = " "
  ))
}

parse_repo_spec <- function(repo_spec) {
  if (!is_string(repo_spec)) {
    abort("`repo_spec` must be a string.")
  }

  repo_spec <- strsplit(repo_spec, split = "/", fixed = TRUE)[[1]]
  if (length(repo_spec) != 2L) {
    abort(c(
      "`repo_spec` must be of the form `owner/repo`.",
      i = "It should be split into exactly two pieces when split on `/`."
    ))
  }

  list(
    owner = repo_spec[[1L]],
    repo = repo_spec[[2L]]
  )
}
