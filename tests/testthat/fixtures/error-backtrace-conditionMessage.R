
options(
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

if (nzchar(Sys.getenv("rlang_interactive"))) {
  options(rlang_interactive = TRUE)
}
options(rlang_trace_format_srcrefs = FALSE)

cnd_header.foobar_error <- function(c, ...) {
  "dispatched!"
}

f <- function() g()
g <- function() h()
h <- function() rlang::abort("", "foobar_error")

f()
