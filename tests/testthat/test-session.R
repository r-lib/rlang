test_that("is_installed() properly checks multiple packages", {
  expect_false(is_installed(c("base", "no.notarealpackagename")))
})

test_that("check_installed() fails if packages are not installed", {
  local_options(rlang_interactive = FALSE)

  .error_call <- quote(my_wrapper())

  expect_snapshot({
    (expect_error(check_installed("_foo")))
    (expect_error(check_installed(c("_foo", "_bar"))))
    (expect_error(check_installed(c("_foo", "_bar"), "to proceed.")))
  })
})

test_that("is_installed() checks minimal versions", {
  expect_error(
    is_installed(c("rlang", "testthat"), version = "0.1"),
    "the same length"
  )
  expect_true(is_installed(c("rlang", "testthat"), version = c("0.1", "0.1")))
  expect_false(is_installed(c("rlang", "testthat"), version = c("100.1", "0.1")))
  expect_false(is_installed(c("rlang", "testthat"), version = c("0.1", "100.1")))
  expect_false(is_installed(c("rlang", "testthis"), version = c("0.1", "0.1")))

  expect_true(is_installed(c("rlang", "testthat"), version = chr(NA, NA)))
  expect_false(is_installed(c("rlang", "testthat"), version = c(NA, "100")))
})

test_that("check_installed() checks minimal versions", {
  local_options(rlang_interactive = FALSE)

  expect_snapshot({
    (expect_error(check_installed("_foo", version = "1.0")))
    (expect_error(check_installed(c("_foo", "_bar"), version = c("1.0", NA))))
    (expect_error(check_installed(c("_foo", "_bar"), version = c(NA, "2.0"))))
    (expect_error(check_installed(c("_foo", "_bar"), "to proceed.", version = c("1.0", "2.0"))))
  })
})

test_that("pnf error is validated", {
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
  is_installed_hook <- function(pkg, ver) {
    if (is_bool(override)) {
      rep_along(pkg, override)
    } else {
      with_options(
        "rlang:::is_installed_hook" = NULL,
        is_installed(pkg, version = ver)
      )
    }
  }
  local_options("rlang:::is_installed_hook" = is_installed_hook)

  test_env <- current_env()
  handle <- function(value, frame, expr) {
    withCallingHandlers(
      rlib_error_package_not_found = function(cnd) {
        override <<- value
        invokeRestart("rlib_restart_package_not_found")
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
