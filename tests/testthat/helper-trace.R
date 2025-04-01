expect_snapshot_trace <- function(
  trace,
  dir = normalizePath(test_path("..")),
  srcrefs = FALSE,
  ...
) {
  expect_snapshot(..., x = {
    "Full"
    print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    "Focused"
    print_focused_trace(trace, dir = dir, srcrefs = srcrefs)
    "Branch"
    print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
  })
}
print_focused_trace <- function(trace, ...) {
  with_options(
    "rlang:::trace_deemph" = function(x) sprintf("<<%s>>", x),
    print(trace, ..., simplify = "none", drop = TRUE)
  )
}

print_highlighted_trace <- function(x, ...) {
  local_options(
    "rlang_trace_format_srcrefs" = FALSE,
    "rlang:::trace_test_highlight" = TRUE
  )
  print(x, ..., simplify = "none", drop = TRUE)
}

expect_trace_length <- function(x, n) {
  expect_equal(trace_length(x), n)
}

expect_equal_trace <- function(x, y) {
  expect_identical(x$parents, y$parents)
  expect_equal(x$calls, y$calls)
}

render_md <- function(file, env = global_env()) {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  out_file <- tempfile(file)
  on.exit(file.remove(out_file))

  rmarkdown::render(
    test_path(file),
    output_format = "md_document",
    output_file = out_file,
    quiet = TRUE,
    envir = env
  )

  readLines(out_file)
}
