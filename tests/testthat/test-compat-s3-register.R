test_that("can register for generics that don't exist", {
  # https://github.com/r-lib/testthat/pull/1407
  skip_if_not_installed("testthat", "3.0.3.9000")

  withr::with_envvar(c(NOT_CRAN = ""), {
    expect_silent(
      s3_register("base::foobarbaz", "class", method = function(...) NULL)
    )
  })

  withr::with_envvar(c(NOT_CRAN = "true"), {
    expect_snapshot({
      (expect_warning(s3_register("base::foobarbaz", "class", method = function(...) NULL)))
    })
  })
})
