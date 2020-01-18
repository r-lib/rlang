
options(
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

library(rlang)

opt <- Sys.getenv("rlang_backtrace_on_error")
if (nzchar(opt)) {
  options(rlang_backtrace_on_error = opt)
}

depth <- as.integer(Sys.getenv("trace_depth"))

if (depth == 1) {
  f <- function() abort("foo")
  f()
} else {
  abort("foo")
}
