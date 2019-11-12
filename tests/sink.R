
library(rlang)


# `inform(file = )` tests cannot be performed with testthat because
# `message` conditions are muffled.

# inform() prints to stdout by default in interactive sessions
local({
  file <- tempfile("inform-file-default")
  do <- function() capture.output(inform("hello"), file = file)

  with_interactive(do(), value = FALSE)
  stopifnot(identical(readLines(file), chr()))

  with_interactive(do())
  stopifnot(identical(readLines(file), "hello"))
})

# inform() prints to file
local({
  file <- tempfile("inform-file-custom")
  inform("foo", file = file)
  stopifnot(identical(readLines(file), "foo"))
})
