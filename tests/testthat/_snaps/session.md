# check_installed() fails if packages are not installed

    Code
      check_installed("rlangFoo")
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The package "rlangFoo" is required.
    Code
      check_installed(c("rlangFoo", "rlangBar"))
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages "rlangFoo" and "rlangBar" are required.
    Code
      check_installed(c("rlangFoo", "rlangBar"), "to proceed.")
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages "rlangFoo" and "rlangBar" are required to proceed.

# is_installed() checks minimal versions

    Code
      is_installed(c("rlang", "testthat"), version = "0.1")
    Condition <rlang_error>
      Error in `is_installed()`:
      ! `version` must be `NULL` or a vector of versions the same length as `pkg`.

# check_installed() checks minimal versions

    Code
      check_installed(c("rlang", "testthat"), version = "0.1")
    Condition <rlang_error>
      Error in `check_installed()`:
      ! `version` must be `NULL` or a vector of versions the same length as `pkg`.
    Code
      check_installed("rlangFoo", version = "1.0")
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The package "rlangFoo" (>= 1.0) is required.
    Code
      check_installed(c("rlangFoo", "rlangBar"), version = c("1.0", NA))
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages "rlangFoo" (>= 1.0) and "rlangBar" are required.
    Code
      check_installed(c("rlangFoo", "rlangBar"), version = c(NA, "2.0"))
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages "rlangFoo" and "rlangBar" (>= 2.0) are required.
    Code
      check_installed(c("rlangFoo", "rlangBar"), "to proceed.", version = c("1.0",
        "2.0"))
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages "rlangFoo" (>= 1.0) and "rlangBar" (>= 2.0) are required to proceed.
    Code
      check_installed(c("rlangFoo (>= 1.0)", "rlangBar (> 2.0)"), "to proceed.")
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The packages "rlangFoo" (>= 1.0) and "rlangBar" (> 2.0) are required to proceed.

# < requirements can't be recovered with restart

    Code
      check_installed("rlang (< 0.1)")
    Condition <rlib_error_package_not_found>
      Error in `foo()`:
      ! The package "rlang" (< 0.1) is required.

# `pkg` is type-checked

    Code
      is_installed(1)
    Condition <rlang_error>
      Error in `is_installed()`:
      ! `pkg` must be a package name or a vector of package names.
    Code
      is_installed(na_chr)
    Condition <rlang_error>
      Error in `is_installed()`:
      ! `pkg` must be a package name or a vector of package names.
    Code
      check_installed(c("foo", ""))
    Condition <rlang_error>
      Error in `check_installed()`:
      ! `pkg` must be a package name or a vector of package names.
    Code
      check_installed(c("foo", "bar"), version = c("1", ""))
    Condition <rlang_error>
      Error in `check_installed()`:
      ! `version` must be `NULL` or a vector of versions the same length as `pkg`.

# pkg_version_info() parses info

    Code
      pkg_version_info("foo (1.0)")
    Condition <rlang_error>
      Error in `caller()`:
      ! Can't parse version in `pkg`.
      x Problematic versions:
      * foo (1.0)
      i Example of expected version format: `rlang (>= 1.0.0)`.
    Code
      pkg_version_info("foo (>= 1.0)", "1.0")
    Condition <rlang_error>
      Error in `caller()`:
      ! Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "foo (>= 1.0)"
    Code
      pkg_version_info(c("foo (!= 1.0)"))
    Condition <rlang_error>
      Error in `caller()`:
      ! `compare` must be one of ">", ">=", "==" ,"<", or "<=".

# pkg_version_info() supports `cmp`

    Code
      pkg_version_info(c("foo", "bar", "baz"), NULL, c(NA, NA, ">="))
    Condition <rlang_error>
      Error in `caller()`:
      ! `version` must be supplied when `compare` is supplied.
    Code
      pkg_version_info(c("foo", "bar", "baz"), c("1", "2", NA), c(NA, NA, ">="))
    Condition <rlang_error>
      Error in `caller()`:
      ! `version` must be supplied when `compare` is supplied.
    Code
      pkg_version_info(c("foo", "bar (>= 2.0)"), c(NA, "2.0"), c(NA, ">="))
    Condition <rlang_error>
      Error in `caller()`:
      ! Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "bar (>= 2.0)"
    Code
      pkg_version_info("foo", "1.0", "!=")
    Condition <rlang_error>
      Error in `caller()`:
      ! `compare` must be one of ">", ">=", "==" ,"<", or "<=".
    Code
      pkg_version_info("bar (== 1.0)", "1.0", "==")
    Condition <rlang_error>
      Error in `caller()`:
      ! Can't supply version in both `pkg` and `version`.
      x Redundant versions:
      * "bar (== 1.0)"

# `action` is checked

    Code
      check_installed("foo", action = "identity")
    Condition <rlang_error>
      Error in `check_installed()`:
      ! `action` must be an R function or `NULL`, not the string "identity".
    Code
      check_installed("foo", action = identity)
    Condition <rlang_error>
      Error in `check_installed()`:
      ! `action` must take a `...` argument.

# `check_installed()` works within `tryCatch(error = )` (#1402, tidyverse/ggplot2#4845)

    Code
      cat(tryCatch(error = function(cnd) NULL, check_installed("rlangFoo")))
    Output
      i The package "rlangFoo" is required.
      x Would you like to install it?

