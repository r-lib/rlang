
options(
  error = rlang::entrace,
  rlang_interactive = TRUE
)

f <- function() g()
g <- function() h()
h <- function() {
  switch(Sys.getenv("rlang_error_kind", unset = "base"),
    base = stop("foo"),
    rlang = rlang::abort("foo")
  )
}

f()

rlang::last_error()

rlang::last_trace()
