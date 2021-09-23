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
test_that("local_options() sets options", {
  old <- local_options(digits = 2L)
  expect_identical(old$digits, old_digits)
  expect_identical(getOption("digits"), 2L)
})
test_that("local_options() restores options", {
  expect_identical(getOption("digits"), old_digits)
})

test_that("trailing newlines are trimmed", {
  expect_identical(strip_trailing_newline("foo"), "foo")
  expect_identical(strip_trailing_newline(""), "")
  expect_identical(strip_trailing_newline("foo\n"), "foo")
  expect_identical(strip_trailing_newline("\n"), "")
})

test_that("source_refs() creates source references", {
  with_srcref("x <- quote({ NULL })")
  attrib_names <- names(attributes(x))
  expect_true(all(c("srcref", "srcfile", "wholeSrcref") %in% attrib_names))
})

test_that("path_trim_prefix() trims path", {
  expect_equal(
    path_trim_prefix("foo/bar/baz.R", 2),
    "bar/baz.R"
  )

  expect_equal(
    path_trim_prefix("foo/bar/baz.R", 3),
    "foo/bar/baz.R"
  )

  expect_equal(
    path_trim_prefix("foo/bar/baz.R", 1),
    "baz.R"
  )
})

test_that("detect_run_starts() works", {
  expect_equal(
    detect_run_starts(chr()),
    lgl()
  )
  expect_equal(
    detect_run_starts("a"),
    TRUE
  )
  expect_equal(
    detect_run_starts(NA),
    NA
  )

  expect_equal(
    detect_run_starts(c("a", "a")),
    c(TRUE, FALSE)
  )
  expect_equal(
    detect_run_starts(c("a", "b")),
    c(TRUE, TRUE)
  )

  expect_equal(
    detect_run_starts(c("a", "b", NA)),
    c(TRUE, TRUE, NA)
  )
  expect_equal(
    detect_run_starts(c("a", NA, "b")),
    c(TRUE, NA, TRUE)
  )
  expect_equal(
    detect_run_starts(c(NA, "a", "b")),
    c(NA, TRUE, TRUE)
  )
})
