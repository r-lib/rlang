# This helper is purposefully used at the end of each test in `test-view.R`
# to only skip the list specific section of each `test_that()` block if
# necessary.
skip_if_no_altlist <- function() {
  skip_if_not(getRversion() >= "4.3.0", message = "Missing ALTLIST support.")
}
