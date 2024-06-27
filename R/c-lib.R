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

  proj_path <- usethis::proj_get()
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
    if (any(fs::file_exists(lib_include_path))) {
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

  if (!has_include_directive(src_path)) {
    usethis::ui_todo("Add to `src/Makevars`:")
    usethis::ui_code_block("PKG_CPPFLAGS = -I./rlang")
  }
  if (!detect_rlang_lib_usage(src_path)) {
    usethis::ui_todo("Include the library with `#include <rlang.h>`.")
    usethis::ui_todo("Call `r_init_library()` from your `.onLoad()` hook.")
  }
}

download_rlang <- function() {
  dest_zip <- fs::file_temp("rlang-src")

  url <- "https://github.com/r-lib/rlang/archive/main.zip"
  utils::download.file(url, dest_zip)

  dest_dir <- fs::file_temp("rlang-src")
  utils::unzip(dest_zip, exdir = dest_dir)

  fs::path(dest_dir, "rlang-main")
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

has_include_directive <- function(src_path) {
  makevars_path <- fs::path(src_path, "Makevars")

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

has_private_accessors <- function() {
  .Call(ffi_has_private_accessors)
}


# cnd.c

format_error_arg <- function(arg) {
  .Call(ffi_format_error_arg, arg)
}


# df.c

alloc_data_frame <- function(n_rows, names, types) {
  .Call(ffi_alloc_data_frame, n_rows, names, types)
}


# dict.c

new_dict <- function(size, prevent_resize = FALSE) {
  .Call(ffi_new_dict, size, prevent_resize)
}
dict_size <- function(dict) {
  length(dict[[2]])
}
dict_resize <- function(dict, size) {
  .Call(ffi_dict_resize, dict, size)
}
dict_poke <- function(dict, key, value) {
  .Call(ffi_dict_poke, dict, key, value)
}
dict_put <- function(dict, key, value) {
  .Call(ffi_dict_put, dict, key, value)
}
dict_del <- function(dict, key) {
  .Call(ffi_dict_del, dict, key)
}
dict_has <- function(dict, key) {
  .Call(ffi_dict_has, dict, key)
}
dict_get <- function(dict, key) {
  .Call(ffi_dict_get, dict, key)
}

dict_as_df_list <- function(dict) {
  .Call(ffi_dict_as_df_list, dict)
}
dict_as_list <- function(dict) {
  .Call(ffi_dict_as_list, dict)
}

#' @export
print.rlang_dict <- function(x, ...) {
  writeLines(sprintf("<rlang/dict: %s>", obj_address(x)))
  writeLines(paste0("size: ", dict_size(x)))
}

new_dict_iterator <- function(dict) {
  .Call(ffi_new_dict_iterator, dict)
}
dict_it_info <- function(it) {
  .Call(ffi_dict_it_info, it)
}
dict_it_next <- function(it) {
  .Call(ffi_dict_next, it)
}


# dyn-array.c

new_dyn_vector <- function(type, capacity) {
  .Call(ffi_new_dyn_vector, type, capacity)
}
new_dyn_array <- function(elt_size, capacity) {
  .Call(ffi_new_dyn_array, elt_size, capacity)
}
dyn_unwrap <- function(x) {
  .Call(ffi_dyn_unwrap, x)
}

dyn_info <- function(x) {
  .Call(ffi_dyn_info, x)
}
dyn_count <- function(x) {
  dyn_info(x)$count
}

dyn_push_back <- function(x, value) {
  .Call(ffi_dyn_push_back, x, value)
}
dyn_push_back_bool <- function(x, value) {
  .Call(ffi_dyn_push_back_bool, x, value)
}
dyn_pop_back <- function(x) {
  .Call(ffi_dyn_pop_back, x)
}
dyn_resize <- function(x, capacity) {
  .Call(ffi_dyn_resize, x, capacity)
}

dyn_lgl_get <- function(x, i) {
  .Call(ffi_dyn_lgl_get, x, i)
}
dyn_int_get <- function(x, i) {
  .Call(ffi_dyn_int_get, x, i)
}
dyn_dbl_get <- function(x, i) {
  .Call(ffi_dyn_dbl_get, x, i)
}
dyn_cpl_get <- function(x, i) {
  .Call(ffi_dyn_cpl_get, x, i)
}
dyn_raw_get <- function(x, i) {
  .Call(ffi_dyn_raw_get, x, i)
}
dyn_chr_get <- function(x, i) {
  .Call(ffi_dyn_chr_get, x, i)
}
dyn_list_get <- function(x, i) {
  .Call(ffi_dyn_list_get, x, i)
}

dyn_lgl_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_lgl_poke, x, i, value))
}
dyn_int_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_int_poke, x, i, value))
}
dyn_dbl_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_dbl_poke, x, i, value))
}
dyn_cpl_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_cpl_poke, x, i, value))
}
dyn_raw_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_raw_poke, x, i, value))
}
dyn_chr_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_chr_poke, x, i, value))
}
dyn_list_poke <- function(x, i, value) {
  invisible(.Call(ffi_dyn_list_poke, x, i, value))
}

dyn_lgl_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_lgl_push_back, x, value))
}
dyn_int_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_int_push_back, x, value))
}
dyn_dbl_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_dbl_push_back, x, value))
}
dyn_cpl_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_cpl_push_back, x, value))
}
dyn_raw_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_raw_push_back, x, value))
}
dyn_chr_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_chr_push_back, x, value))
}
dyn_list_push_back <- function(x, value) {
  invisible(.Call(ffi_dyn_list_push_back, x, value))
}

# https://github.com/r-lib/rlang/issues/1556
has_size_one_bool <- function() {
  .Call(ffi_has_size_one_bool)
}

#' @export
print.rlang_dyn_array <- function(x, ...) {
  writeLines(sprintf("<rlang/dyn_array: %s>", obj_address(x)))

  info <- dyn_info(x)
  writeLines(paste0("count: ", info$count))
  writeLines(paste0("capacity: ", info$capacity))
  writeLines(paste0("growth_factor: ", info$growth_factor))
  writeLines(paste0("type: ", info$type))
  writeLines(paste0("elt_byte_size: ", info$elt_byte_size))
}


# dyn-list-of.c

new_dyn_list_of <- function(type, capacity, width) {
  .Call(ffi_new_dyn_list_of, type, capacity, width)
}
lof_info <- function(lof) {
  .Call(ffi_lof_info, lof)
}
lof_unwrap <- function(lof) {
  .Call(ffi_lof_unwrap, lof)
}

lof_push_back <- function(lof) {
  .Call(ffi_lof_push_back, lof)
}
lof_arr_push_back <- function(lof, i, value) {
  .Call(ffi_lof_arr_push_back, lof, i, value)
}


# obj.c

has_local_precious_list <- function() {
  .Call(ffi_has_local_precious_list)
}
use_local_precious_list <- function(x) {
  .Call(ffi_use_local_precious_list, x)
}


# sexp.c

rlang_precious_dict <- function() {
  .Call(ffi_precious_dict)
}
rlang_preserve <- function(x) {
  .Call(ffi_preserve, x)
}
rlang_unpreserve <- function(x) {
  .Call(ffi_unpreserve, x)
}


# session.c

getppid <- function() {
  .Call(ffi_getppid)
}


# tests.c

c_tests <- function() {
  .Call(ffi_c_tests)
}
run_c_test <- function(ptr) {
  .Call(ffi_run_c_test, ptr)
}


# vec.c

list_compact <- function(x) {
  .Call(ffi_list_compact, x)
}

vec_resize <- function(x, n) {
  .Call(ffi_vec_resize, x, n)
}


# walk.c

sexp_iterate <- function(x, fn) {
  do.call(".Call", list(ffi_sexp_iterate, x, fn))
}
