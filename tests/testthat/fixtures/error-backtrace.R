
f <- function() g()
g <- function() h()
h <- function() rlang::abort("Error message")

f()
