context("utils")

test_that("locale setters report old locale", {
  tryCatch(
    old <- suppressMessages(poke_mbcs_locale()),
    warning = function(e) skip("Cannot set MBCS locale")
  )

  mbcs <- suppressMessages(poke_latin1_locale())
  suppressMessages(Sys.setlocale("LC_CTYPE", old))
  expect_true(tolower(mbcs) %in% tolower(c("ja_JP.SJIS", "English_United States.932")))
})

old_digits <- getOption("digits")
test_that("scoped_options() sets options", {
  old <- scoped_options(digits = 2L)
  expect_identical(old$digits, old_digits)
  expect_identical(getOption("digits"), 2L)
})
test_that("scoped_options() restores options", {
  expect_identical(getOption("digits"), old_digits)
})
