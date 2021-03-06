
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
  local_bindings(..., .env = global_env(), .frame = .frame)
}
with_methods <- function(.expr, ...) {
  local_methods(...)
  .expr
}

# Some backtrace tests use Rscript, which requires the last version of
# the backtrace code to be installed locally
skip_if_stale_backtrace <- local({
  current_backtrace_ver <- "1.0.1"

  ver <- system.file("backtrace-ver", package = "rlang")
  has_stale_backtrace <- ver == "" || !identical(readLines(ver), current_backtrace_ver)

  function() {
    skip_if(has_stale_backtrace)
  }
})

skip_if_big_endian <- function() {
  skip_if(
    identical(.Platform$endian, "big"),
    "Skipping on big-endian platform."
  )
}

Rscript <- function(args, ...) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    args,
    ...,
    stdout = TRUE,
    stderr = TRUE
  ))

  list(
    out = unstructure(out),
    status = attr(out, "status")
  )
}
run <- function(code) {
  out <- Rscript(shQuote(c("--vanilla", "-e", code)))
  cat_line(out$out)
}

expect_reference <- function(object, expected) {
  expect_true(is_reference(object, expected))
}

rlang_compats <- function(fn) {
  list(
    .rlang_compat(fn),
    .rlang_compat(fn, try_rlang = FALSE)
  )
}

# Deterministic behaviour on old R versions
data.frame <- function(..., stringsAsFactors = FALSE) {
  base::data.frame(..., stringsAsFactors = stringsAsFactors)
}

skip_if_not_windows <- function() {
  system <- tolower(Sys.info()[["sysname"]])
  skip_if_not(is_string(system, "windows"), "Not on Windows")
}
