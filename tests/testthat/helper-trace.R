
expect_known_trace_output <- function(trace,
                                      file,
                                      dir = normalizePath(test_path("..")),
                                      srcrefs = FALSE) {
  expect_known_output(file = test_path(file), {
    cat("Full:\n")
    print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    cat("\nBranch:\n")
    print(trace, simplify = "branch", dir = dir, srcrefs = srcrefs)
  })
}

expect_trace_length <- function(x, n) {
  expect_equal(trace_length(x), n)
}

expect_equal_trace <- function(x, y) {
  expect_identical(x$parents, y$parents)
  expect_equal(x$calls, y$calls)
}
