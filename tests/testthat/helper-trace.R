
expect_known_trace_output <- function(trace, file, dir, srcrefs = FALSE) {
  expect_known_output(file = test_path(file), {
    cat("Full:\n")
    print(trace, simplify = "none", dir = dir, srcrefs = srcrefs)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", dir = dir, srcrefs = srcrefs)
    cat("\nTrail:\n")
    print(trace, simplify = "trail", dir = dir, srcrefs = srcrefs)
  })
}
