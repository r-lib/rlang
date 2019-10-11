
zap_attributes <- function(x) {
  attributes(x) <- NULL
  x
}
zap_srcref_attributes <- function(x) {
  attr(x, "srcref") <- NULL
  attr(x, "srcfile") <- NULL
  attr(x, "wholeSrcref") <- NULL
  x
}

run_script <- function(file, envvars = chr()) {
  skip_on_os("windows")

  # Suppress non-zero exit warnings
  suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", file),
    stdout = TRUE,
    stderr = TRUE,
    env = envvars
  ))
}

local_methods <- function(..., .frame = caller_env()) {
  scoped_bindings(..., .env = global_env(), .frame = .frame)
}
