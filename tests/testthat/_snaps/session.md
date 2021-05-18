# check_installed() fails if packages are not installed

    Code
      (expect_error(check_installed("_foo")))
    Output
      <error/rlang_error>
      The package `_foo` is required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"))))
    Output
      <error/rlang_error>
      The packages `_foo` and `_bar` are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), "to proceed.")))
    Output
      <error/rlang_error>
      The packages `_foo` and `_bar` are required to proceed.

# check_installed() checks minimal versions

    Code
      (expect_error(check_installed("_foo", version = "1.0")))
    Output
      <error/rlang_error>
      The package `_foo` (>= 1.0) is required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), version = c("1.0", NA))))
    Output
      <error/rlang_error>
      The packages `_foo` (>= 1.0) and `_bar` are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), version = c(NA, "2.0"))))
    Output
      <error/rlang_error>
      The packages `_foo` and `_bar` (>= 2.0) are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), "to proceed.", version = c(
        "1.0", "2.0"))))
    Output
      <error/rlang_error>
      The packages `_foo` (>= 1.0) and `_bar` (>= 2.0) are required to proceed.

