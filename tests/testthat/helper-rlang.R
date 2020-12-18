
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
  current_backtrace_ver <- "1.0.0"

  ver <- system.file("backtrace-ver", package = "rlang")
  has_stale_backtrace <- ver == "" || !identical(readLines(ver), current_backtrace_ver)

  function() {
    skip_if(has_stale_backtrace)
  }
})

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

expect_reference <- function(object, expected) {
  expect_true(is_reference(object, expected))
}
