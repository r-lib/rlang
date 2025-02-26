options(
  error = rlang::entrace,
  rlang_interactive = TRUE,
  crayon.enabled = FALSE,
  cli.unicode = FALSE,
  cli.dynamic = FALSE,
  useFancyQuotes = FALSE,
  OutDec = ".",
  width = 80L
)

f <- function() g()
g <- function() h()
h <- function() {
  switch(
    Sys.getenv("rlang_error_kind", unset = "base"),
    base = stop("foo"),
    rlang = rlang::abort("foo")
  )
}

f()

rlang::last_error()

rlang::last_trace()
