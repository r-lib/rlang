# check_installed() fails if packages are not installed

    Code
      (expect_error(check_installed("rlangFoo")))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The package `rlangFoo` is required.
    Code
      (expect_error(check_installed(c("rlangFoo", "rlangBar"))))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages `rlangFoo` and `rlangBar` are required.
    Code
      (expect_error(check_installed(c("rlangFoo", "rlangBar"), "to proceed.")))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages `rlangFoo` and `rlangBar` are required to proceed.

# is_installed() checks minimal versions

    Code
      (expect_error(is_installed(c("rlang", "testthat"), version = "0.1"),
      "the same length"))
    Output
      <error/rlang_error>
      Error in `is_installed()`:
      ! `version` must be `NULL` or a vector of versions the same length as `pkg`.

# check_installed() checks minimal versions

    Code
      (expect_error(check_installed(c("rlang", "testthat"), version = "0.1")))
    Output
      <error/rlang_error>
      Error in `check_installed()`:
      ! `version` must be `NULL` or a vector of versions the same length as `pkg`.
    Code
      (expect_error(check_installed("rlangFoo", version = "1.0")))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The package `rlangFoo` (>= 1.0) is required.
    Code
      (expect_error(check_installed(c("rlangFoo", "rlangBar"), version = c("1.0", NA)))
      )
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages `rlangFoo` (>= 1.0) and `rlangBar` are required.
    Code
      (expect_error(check_installed(c("rlangFoo", "rlangBar"), version = c(NA, "2.0")))
      )
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages `rlangFoo` and `rlangBar` (>= 2.0) are required.
    Code
      (expect_error(check_installed(c("rlangFoo", "rlangBar"), "to proceed.",
      version = c("1.0", "2.0"))))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages `rlangFoo` (>= 1.0) and `rlangBar` (>= 2.0) are required to proceed.
    Code
      (expect_error(check_installed(c("rlangFoo (>= 1.0)", "rlangBar (> 2.0)"),
      "to proceed.")))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages `rlangFoo` (>= 1.0) and `rlangBar` (> 2.0) are required to proceed.

# < requirements can't be recovered with restart

    Code
      (expect_error(check_installed("rlang (< 0.1)")))
    Output
      <error/rlib_error_package_not_found>
      Error in `foo()`:
      ! The package `rlang` (< 0.1) is required.

# `pkg` is type-checked

    Code
      (expect_error(is_installed(1)))
    Output
      <error/rlang_error>
      Error in `is_installed()`:
      ! `pkg` must be a package name or a vector of package names.
    Code
      (expect_error(is_installed(na_chr)))
    Output
      <error/rlang_error>
      Error in `is_installed()`:
      ! `pkg` must be a package name or a vector of package names.
    Code
      (expect_error(check_installed(c("foo", ""))))
    Output
      <error/rlang_error>
      Error in `check_installed()`:
      ! `pkg` must be a package name or a vector of package names.
    Code
      (expect_error(check_installed(c("foo", "bar"), version = c("1", ""))))
    Output
      <error/rlang_error>
      Error in `check_installed()`:
      ! `version` must be `NULL` or a vector of versions the same length as `pkg`.

# pkg_version_info() parses info

    Code
      (expect_error(pkg_version_info("foo 1.0"), "valid"))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! Must supply valid package names.
      x Problematic names:
      * "foo 1.0"
    Code
      (expect_error(pkg_version_info("foo (1.0)"), "parse"))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! Can't parse version in `pkg`.
    Code
      (expect_error(pkg_version_info("foo (>= 1.0)", "1.0"), "both"))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "foo (>= 1.0)"
    Code
      (expect_error(pkg_version_info(c("foo (!= 1.0)"))))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! `compare` must be one of ">", ">=", "<", or "<=".

# pkg_version_info() supports `cmp`

    Code
      err(pkg_version_info(c("foo", "bar", "baz"), NULL, c(NA, NA, ">=")))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! `version` must be supplied when `compare` is supplied.
    Code
      err(pkg_version_info(c("foo", "bar", "baz"), c("1", "2", NA), c(NA, NA, ">=")))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! `version` must be supplied when `compare` is supplied.
    Code
      err(pkg_version_info(c("foo", "bar (>= 2.0)"), c(NA, "2.0"), c(NA, ">=")))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "bar (>= 2.0)"
    Code
      err(pkg_version_info("foo", "1.0", "!="))
    Output
      <error/rlang_error>
      Error in `caller()`:
      ! `compare` must be one of ">", ">=", "<", or "<=".

# `action` is checked

    Code
      err(check_installed("foo", action = "identity"))
    Output
      <error/rlang_error>
      Error in `check_installed()`:
      ! `action` must be `NULL` or a function, not a string.
    Code
      err(check_installed("foo", action = identity))
    Output
      <error/rlang_error>
      Error in `check_installed()`:
      ! `action` must take a `...` argument.

