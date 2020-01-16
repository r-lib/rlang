
f <- function() tryCatch(g())
g <- function() h()
h <- function() rlang::abort("Error message")

tryCatch(
  f(),
  error = function(cnd) rlang::cnd_signal(cnd)
)
