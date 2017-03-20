context("utils")

test_that("locale setters report old locale", {
  skip_on_cran() # Probably not portable
  skip_on_travis() # Limited locales available
  old <- suppressMessages(mut_mbcs_locale())
  mbcs <- suppressMessages(mut_latin1_locale())
  suppressMessages(Sys.setlocale("LC_CTYPE", old))
  expect_true(tolower(mbcs) %in% tolower(c("ja_JP.SJIS", "English_United States.932")))
})
