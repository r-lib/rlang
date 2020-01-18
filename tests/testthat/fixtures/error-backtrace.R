
options(
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

opt <- Sys.getenv("rlang_backtrace_on_error")
if (nzchar(opt)) {
  options(rlang_backtrace_on_error = opt)
}
if (nzchar(Sys.getenv("rlang_interactive"))) {
  options(rlang_interactive = TRUE)
}
options(rlang_trace_format_srcrefs = FALSE)

f <- function() tryCatch(g())
g <- function() h()
h <- function() rlang::abort("Error message")

f()
