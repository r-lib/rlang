library(rlang)

# inform() prints to file
local({
  file <- tempfile("inform-file-custom")
  inform("foo", .file = file)
  stopifnot(identical(readLines(file), "foo"))
})
