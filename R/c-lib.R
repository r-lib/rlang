#' Import or update the rlang C library
#'
#' @description
#'
#' The rlang library is downloaded from the development version on
#' github by default. Set the `RLANG_PATH` environment variable to a
#' local path to copy rlang from a local source.
#'
#' Set the `RLANG_LIB_NO_PROMPT` environment variable to `"true"` to
#' disable the prompts about overwriting an existing library.
#'
#' @noRd
use_rlang_c_library <- function() {
  check_installed(c("fs", "usethis"), "to install the rlang C library.")

  rlang_path <- Sys.getenv("RLANG_PATH")
  if (!nzchar(rlang_path)) {
    rlang_path <- download_rlang()
    on.exit(fs::dir_delete(rlang_path))
  }

  check_rlang(rlang_path)
  rlang_lib_path <- fs::path(rlang_path, "src", "rlang")
  rlang_lib_include_path <- fs::path(rlang_path, "src", "rlang.c")

  proj_path <- usethis::proj_path()
  if (is_rlang_dir(proj_path)) {
    abort(c(
      "Can't update rlang from itself.",
      i = "Did you forget to change project?"
    ))
  }

  src_path <- fs::path(proj_path, "src")
  lib_path <- fs::path(src_path, "rlang")
  lib_include_path <- fs::path(src_path, "rlang.c")

  has_library <- any(fs::file_exists(c(lib_path, lib_include_path)))

  if (has_library) {
    no_prompt <- tolower(Sys.getenv("RLANG_LIB_NO_PROMPT"))
    if (!is_string(no_prompt, "true")) {
      prompt <- paste("Remove existing library in", src_path)
      if (!usethis::ui_yeah(prompt)) {
        invokeRestart("abort")
      }
    }

    if (fs::file_exists(lib_path)) {
      fs::file_delete(lib_path)
    }
    if (fs::file_exists(lib_include_path)) {
      fs::file_delete(lib_include_path)
    }
  }

  fs::dir_copy(rlang_lib_path, lib_path)
  fs::file_copy(rlang_lib_include_path, lib_include_path)

  if (has_library) {
    usethis::ui_done("Updated rlang library.")
  } else {
    usethis::ui_done("Installed rlang library to `src/rlang`.")
  }

  makevars_path <- fs::path(src_path, "Makevars")
  if (!has_include_directive(makevars_path)) {
    usethis::ui_todo("Add to `src/Makvars:`")
    usethis::ui_code_block("PKG_CPPFLAGS = -I./rlang")
  }

  if (!detect_rlang_lib_usage(src_path)) {
    usethis::ui_todo("Include the library with `#include <rlang.h>`.")
    usethis::ui_todo("Call `r_init_library()` from your `.onLoad()` hook.")
  }
}

download_rlang <- function() {
  dest_zip <- fs::file_temp("rlang-src")

  url <- "https://github.com/r-lib/rlang/archive/master.zip"
  utils::download.file(url, dest_zip)

  dest_dir <- fs::file_temp("rlang-src")
  utils::unzip(dest_zip, exdir = dest_dir)

  fs::path(dest_dir, "rlang-master")
}

check_rlang <- function(path) {
  if (!is_rlang_dir(path)) {
    abort("Can't find rlang in path.")
  }
}
is_rlang_dir <- function(path) {
  lib_path <- fs::path(path, "src", "rlang")
  desc_path <- fs::path(path, "DESCRIPTION")

  if (!fs::dir_exists(lib_path)) {
    return(FALSE)
  }
  if (!fs::file_exists(desc_path)) {
    return(FALSE)
  }

  desc <- readLines(desc_path, n = 1)
  if (!is_string(desc, "Package: rlang")) {
    return(FALSE)
  }

  TRUE
}

has_include_directive <- function(makevars_path) {
  if (!fs::file_exists(makevars_path)) {
    return(FALSE)
  }

  makevars_lines <- readLines(makevars_path)
  any(grepl("PKG_CPPFLAGS.*-I.*rlang", makevars_lines))
}

detect_rlang_lib_usage <- function(src_path) {
  src_files <- fs::dir_ls(src_path, glob = "*.c", recurse = TRUE)
  src_files <- src_files[!grepl("/src/rlang/", src_files)]

  has_include <- FALSE
  has_init <- FALSE

  for (file in src_files) {
    lines <- readLines(file)

    if (any(grepl("#include <rlang.h>", lines))) {
      has_include <- TRUE
    }
    if (any(grepl("r_init_library", lines))) {
      has_init <- TRUE
    }

    if (has_include && has_init) {
      return(TRUE)
    }
  }

  FALSE
}


# dict.c

new_dict <- function(size, prevent_resize = FALSE) {
  .Call(rlang_new_dict, size, prevent_resize)
}
dict_size <- function(dict) {
  length(dict[[2]][[1]])
}
dict_resize <- function(dict, size) {
  .Call(rlang_dict_resize, dict, size)
}
dict_put <- function(dict, key, value) {
  .Call(rlang_dict_put, dict, key, value)
}
dict_del <- function(dict, key) {
  .Call(rlang_dict_del, dict, key)
}
dict_has <- function(dict, key) {
  .Call(rlang_dict_has, dict, key)
}
dict_get <- function(dict, key) {
  .Call(rlang_dict_get, dict, key)
}
