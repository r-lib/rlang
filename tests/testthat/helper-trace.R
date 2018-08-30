
expect_known_trace_output <- function(trace, file) {
  expect_known_output(file = test_path(file), {
    cat("Full:\n")
    print(trace, simplify = "none", srcrefs = FALSE)
    cat("\nCollapsed:\n")
    print(trace, simplify = "collapse", srcrefs = FALSE)
    cat("\nTrail:\n")
    print(trace, simplify = "trail", srcrefs = FALSE)
  })
}
