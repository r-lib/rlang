
options(
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

if (nzchar(Sys.getenv("rlang_interactive"))) {
  options(rlang_interactive = TRUE)
}

f <- function() tryCatch(g())
g <- function() h()
h <- function() rlang::abort("Error message")

tryCatch(
  f(),
  error = function(cnd) rlang::cnd_signal(cnd)
)
