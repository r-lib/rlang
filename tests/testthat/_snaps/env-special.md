# check_installed() fails if packages are not installed

    Code
      (expect_error(check_installed("_foo")))
    Output
      <error/rlang_error>
      The `_foo` package is required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"))))
    Output
      <error/rlang_error>
      The `_foo` and `_bar` packages are required.
    Code
      (expect_error(check_installed(c("_foo", "_bar"), "to proceed.")))
    Output
      <error/rlang_error>
      The `_foo` and `_bar` packages are required to proceed.

