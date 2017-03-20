context("utils")

test_that("locale setters report old locale", {
  tryCatch(
    old <- suppressMessages(mut_mbcs_locale()),
    warning = function(e) skip("Cannot set MBCS locale")
  )

  mbcs <- suppressMessages(mut_latin1_locale())
  suppressMessages(Sys.setlocale("LC_CTYPE", old))
  expect_true(tolower(mbcs) %in% tolower(c("ja_JP.SJIS", "English_United States.932")))
})
