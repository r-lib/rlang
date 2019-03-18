
zap_attributes <- function(x) {
  attributes(x) <- NULL
  x
}
zap_srcref_attributes <- function(x) {
  attr(x, "srcref") <- NULL
  attr(x, "srcfile") <- NULL
  attr(x, "wholeSrcref") <- NULL
  x
}

expect_known_output <- function(...) {
  skip_on_cran()
  testthat::expect_known_output(...)
}
