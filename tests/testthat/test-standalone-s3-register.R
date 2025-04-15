test_that("can register for generics that don't exist", {
  withr::with_envvar(c(NOT_CRAN = ""), {
    expect_silent(
      s3_register("testthat::foobarbaz", "class", method = function(...) NULL)
    )
  })

  # https://github.com/r-lib/testthat/pull/1401
  skip_if_not_installed("testthat", "3.0.4.9000")

  withr::with_envvar(c(NOT_CRAN = "true"), {
    expect_snapshot({
      (expect_warning(s3_register(
        "testthat::foobarbaz",
        "class",
        method = function(...) NULL
      )))
    })
  })
})
