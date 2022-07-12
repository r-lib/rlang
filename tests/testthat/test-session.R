test_that("is_installed() properly checks multiple packages", {
  expect_false(is_installed(c("base", "no.notarealpackagename")))
})

test_that("check_installed() fails if packages are not installed", {
  local_options(rlang_interactive = FALSE)
  local_error_call(call("foo"))

  expect_snapshot({
    (expect_error(check_installed("rlangFoo")))
    (expect_error(check_installed(c("rlangFoo", "rlangBar"))))
    (expect_error(check_installed(c("rlangFoo", "rlangBar"), "to proceed.")))
  })
})

test_that("is_installed() checks minimal versions", {
  expect_snapshot({
    (expect_error(
      is_installed(c("rlang", "testthat"), version = "0.1"),
      "the same length"
    ))
  })
  expect_true(is_installed(c("rlang", "testthat"), version = c("0.1", "0.1")))
  expect_false(is_installed(c("rlang", "testthat"), version = c("100.1", "0.1")))
  expect_false(is_installed(c("rlang", "testthat"), version = c("0.1", "100.1")))
  expect_false(is_installed(c("rlang", "testthis"), version = c("0.1", "0.1")))

  expect_true(is_installed(c("rlang", "testthat"), version = chr(NA, NA)))
  expect_false(is_installed(c("rlang", "testthat"), version = c(NA, "100")))

  expect_true(is_installed(c("rlang (>= 0.1)", "testthat (>= 0.1)")))
  expect_false(is_installed(c("rlang (>= 100.1)", "testthat (>= 0.1)")))
  expect_false(is_installed(c("rlang (<= 0.4.0)", "testthat (>= 0.1)")))
})

test_that("check_installed() checks minimal versions", {
  local_options(rlang_interactive = FALSE)
  local_error_call(call("foo"))

  expect_null(check_installed(c("rlang (>= 0.1)", "testthat (>= 0.1)")))

  expect_snapshot({
    (expect_error(check_installed(c("rlang", "testthat"), version = "0.1")))
    (expect_error(check_installed("rlangFoo", version = "1.0")))
    (expect_error(check_installed(c("rlangFoo", "rlangBar"), version = c("1.0", NA))))
    (expect_error(check_installed(c("rlangFoo", "rlangBar"), version = c(NA, "2.0"))))
    (expect_error(check_installed(c("rlangFoo", "rlangBar"), "to proceed.", version = c("1.0", "2.0"))))
    (expect_error(check_installed(c("rlangFoo (>= 1.0)", "rlangBar (> 2.0)"), "to proceed.")))
  })
})

test_that("< requirements can't be recovered with restart", {
  local_options(rlang_interactive = TRUE)
  local_error_call(call("foo"))
  expect_snapshot({
    (expect_error(check_installed("rlang (< 0.1)")))
  })
})

test_that("pnf error is validated", {
  # No need to revalidate unless we export it
  expect_true(TRUE)
  return()

  expect_pnf <- function(out, pkg, ver) {
    expect_s3_class(out, "rlib_error_package_not_found")
    expect_equal(out$pkg, pkg)
    expect_equal(out$version, ver)
  }
  expect_pnf(new_error_package_not_found("foo"), "foo", NULL)
  expect_pnf(new_error_package_not_found("foo", "1.0"), "foo", "1.0")
  expect_pnf(new_error_package_not_found(c("foo", "bar"), c("1.0", "1.0")), c("foo", "bar"), c("1.0", "1.0"))

  expect_error(
    new_error_package_not_found(chr()),
    "at least one package"
  )
  expect_error(
    new_error_package_not_found(c("foo", "bar"), "1.0"),
    "as long as `pkg`"
  )
})

test_that("can handle check-installed", {
  local_interactive()

  # Override `is_installed()` results
  override <- NULL
  is_installed_hook <- function(pkg, ver, cmp) {
    if (is_bool(override)) {
      rep_along(pkg, override)
    } else {
      with_options(
        "rlang:::is_installed_hook" = NULL,
        is_installed(pkg, version = ver, compare = cmp)
      )
    }
  }
  local_options("rlang:::is_installed_hook" = is_installed_hook)

  test_env <- current_env()
  handle <- function(value, frame, expr) {
    withCallingHandlers(
      rlib_error_package_not_found = function(cnd) {
        override <<- value
        if (!is_null(findRestart("rlib_restart_package_not_found"))) {
          invokeRestart("rlib_restart_package_not_found")
        }
      },
      expr
    )
  }

  override <- NULL
  expect_no_error(
    handle(
      TRUE,
      test_env,
      check_installed(c("foo", "bar"), version = c("1.0", "2.0"))
    )
  )

  override <- NULL
  expect_error(
    handle(
      FALSE,
      test_env,
      check_installed(c("foo", "bar"), version = c("1.0", "2.0"))
    ),
    "are required"
  )
})

test_that("`pkg` is type-checked", {
  expect_snapshot({
    (expect_error(is_installed(1)))
    (expect_error(is_installed(na_chr)))
    (expect_error(check_installed(c("foo", ""))))
    (expect_error(check_installed(c("foo", "bar"), version = c("1", ""))))
  })
})

test_that("pkg_version_info() parses info", {
  local_error_call(call("caller"))

  pkg <- c("foo (>= 1.0)", "bar", "baz (> 3.0)")
  out <- pkg_version_info(pkg, NULL)

  expect_equal(out, data_frame(
    pkg = c("foo", "bar", "baz"),
    cmp = c(">=", NA, ">"),
    ver = c("1.0", NA, "3.0")
  ))

  pkg <- c("foo (>= 1.0)", "bar", "baz (> 3.0)", "quux")
  out <- pkg_version_info(pkg, c(NA, "2.0", NA, NA))

  expect_equal(out, data_frame(
    pkg = c("foo", "bar", "baz", "quux"),
    cmp = c(">=", ">=", ">", NA),
    ver = c("1.0", "2.0", "3.0", NA)
  ))

  pkg <- c("foo (>= 1.0)", "bar", "baz (> 3.0)", "quux")
  out <- pkg_version_info(pkg, c(NA, "2.0", NA, "4.0"))

  expect_equal(out, data_frame(
    pkg = c("foo", "bar", "baz", "quux"),
    cmp = c(">=", ">=", ">", ">="),
    ver = c("1.0", "2.0", "3.0", "4.0")
  ))

  expect_snapshot({
    (expect_error(pkg_version_info("foo (1.0)"), "parse"))
    (expect_error(pkg_version_info("foo (>= 1.0)", "1.0"), "both"))
    (expect_error(pkg_version_info(c("foo (!= 1.0)"))))
  })
})

test_that("pkg_version_info() supports `cmp`", {
  local_error_call(call("caller"))

  pkg <- c("foo", "bar", "baz")
  out <- pkg_version_info(pkg, c("1.0", "2.0", "3.0"), c(NA, NA, "<"))

  expect_equal(out, data_frame(
    pkg = c("foo", "bar", "baz"),
    cmp = c(">=", ">=", "<"),
    ver = c("1.0", "2.0", "3.0")
  ))

  expect_snapshot({
    err(pkg_version_info(c("foo", "bar", "baz"), NULL, c(NA, NA, ">=")))
    err(pkg_version_info(c("foo", "bar", "baz"), c("1", "2", NA), c(NA, NA, ">=")))
    err(pkg_version_info(c("foo", "bar (>= 2.0)"), c(NA, "2.0"), c(NA, ">=")))
    err(pkg_version_info("foo", "1.0", "!="))
  })
})

test_that("`action` is checked", {
  expect_snapshot({
    err(check_installed("foo", action = "identity"))
    err(check_installed("foo", action = identity))
  })
})

test_that("`check_installed()` works within `tryCatch(error = )` (#1402, tidyverse/ggplot2#4845)", {
  local_options("rlang:::check_installed_test_hook" = TRUE)
  local_interactive()

  expect_snapshot({
    cat(tryCatch(
      error = function(cnd) NULL,
      check_installed("rlangFoo")
    ))
  })
})

test_that("is_installed('base') works (#1434)", {
  r_ver <- as.character(getRversion())

  expect_true(is_installed("base"))
  expect_true(is_installed("base", version = "0.0.1"))
  expect_true(is_installed("base", version = r_ver))
  expect_false(is_installed("base", version = "999.9.9"))

  skip_if_not(is_string(Sys.getenv("R_DEFAULT_PACKAGES"), ""))

  for (pkg in peek_option("defaultPackages")) {
    expect_true(is_installed(pkg))
    expect_true(is_installed(pkg, version = "0.0.1"))
    expect_true(is_installed(pkg, version = r_ver))
    expect_false(is_installed(pkg, version = "999.9.9"))
  }
})

test_that("is_installed() allows irregular package names", {
  # Consistently with `loadNamespace()`
  expect_false(is_installed("foo 1.0"))
  expect_false(is_installed("::", version = "1.0"))
})
