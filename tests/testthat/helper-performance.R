# From r-lib/bench
with_memory_prof <- function(expr) {
  f <- tempfile()
  on.exit(unlink(f))

  tryCatch(
    utils::Rprofmem(f, threshold = 1),
    error = function(...) skip("Can't profile memory on this system.")
  )
  on.exit(utils::Rprofmem(NULL), add = TRUE)

  res <- force(expr)
  utils::Rprofmem(NULL)

  bytes <- parse_allocations(f)$bytes
  bytes <- sum(bytes, na.rm = TRUE)
  new_bytes(bytes)
}
parse_allocations <- function(filename) {
  if (!is_installed("profmem")) {
    testthat::skip("profmem must be installed.")
  }
  readRprofmem <- env_get(ns_env("profmem"), "readRprofmem")

  tryCatch(
    readRprofmem(filename),
    error = function(cnd) {
      testthat::skip(sprintf(
        "Memory profiling failed: %s",
        conditionMessage(cnd)
      ))
    }
  )
}
