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


# df.c

alloc_data_frame <- function(n_rows, names, types) {
  .Call(c_ptr_alloc_data_frame, n_rows, names, types)
}


# dict.c

new_dict <- function(size, prevent_resize = FALSE) {
  .Call(rlang_new_dict, size, prevent_resize)
}
dict_size <- function(dict) {
  length(dict[[2]])
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

#' @export
print.rlang_dict <- function(x, ...) {
  writeLines(sprintf("<rlang/dict: %s>", sexp_address(x)))
  writeLines(paste0("size: ", dict_size(x)))
}

new_dict_iterator <- function(dict) {
  .Call(c_ptr_new_dict_iterator, dict)
}
dict_it_info <- function(it) {
  .Call(c_ptr_dict_it_info, it)
}
dict_it_next <- function(it) {
  .Call(c_ptr_dict_it_next, it)
}


# dyn-array.c

new_dyn_vector <- function(type, capacity) {
  .Call(c_ptr_new_dyn_vector, type, capacity)
}
new_dyn_array <- function(elt_size, capacity) {
  .Call(c_ptr_new_dyn_array, elt_size, capacity)
}
arr_unwrap <- function(arr) {
  .Call(c_ptr_arr_unwrap, arr)
}

arr_info <- function(arr) {
  .Call(c_ptr_arr_info, arr)
}

arr_push_back <- function(arr, x) {
  .Call(c_ptr_arr_push_back, arr, x)
}
arr_push_back_bool <- function(arr, x) {
  .Call(c_ptr_arr_push_back_bool, arr, x)
}
arr_pop_back <- function(arr) {
  .Call(c_ptr_arr_pop_back, arr)
}
arr_resize <- function(arr, capacity) {
  .Call(c_ptr_arr_resize, arr, capacity)
}

#' @export
print.rlang_dyn_array <- function(x, ...) {
  writeLines(sprintf("<rlang/dyn_array: %s>", sexp_address(x)))

  info <- arr_info(x)
  writeLines(paste0("count: ", info$count))
  writeLines(paste0("capacity: ", info$capacity))
  writeLines(paste0("growth_factor: ", info$growth_factor))
  writeLines(paste0("type: ", info$type))
  writeLines(paste0("elt_byte_size: ", info$elt_byte_size))
}


# sexp.c

rlang_precious_dict <- function() {
  .Call(c_ptr_precious_dict)
}
rlang_preserve <- function(x) {
  .Call(c_ptr_preserve, x)
}
rlang_unpreserve <- function(x) {
  .Call(c_ptr_unpreserve, x)
}


# vec.c

list_compact <- function(x) {
  .Call(c_ptr_list_compact, x)
}

vec_resize <- function(x, n) {
  .Call(c_ptr_vec_resize, x, n) 
}
