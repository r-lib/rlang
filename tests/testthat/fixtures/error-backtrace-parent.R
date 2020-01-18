
options(
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

if (nzchar(Sys.getenv("rlang_interactive"))) {
  options(rlang_interactive = TRUE)
}
options(rlang_trace_format_srcrefs = FALSE)

f <- function() g()
g <- function() h()
h <- function() rlang::abort("foo")

a <- function() b()
b <- function() c()
c <- function() {
  tryCatch(
    f(),
    error = function(err) rlang::abort("bar", parent = err)
  )
}

a()
