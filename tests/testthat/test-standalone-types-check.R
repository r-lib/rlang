test_that("`check_character()` checks", {
  expect_null(check_character(""))
  expect_null(check_character(na_chr))
  expect_null(check_character(c("a", NA)))
  expect_null(check_character(chr()))
  expect_null(check_character("foo"))
  expect_null(check_character(letters))
  expect_null(check_character(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_character))
    err(checker(NULL, check_character))
    err(checker(NA, check_character))
    err(checker(1, check_character))
    err(checker(list("foo", "bar"), check_character, allow_null = TRUE))
    err(checker(c("a", NA), check_character, allow_na = FALSE))
  })
})

test_that("`check_logical()` checks", {
  expect_null(check_logical(TRUE))
  expect_null(check_logical(FALSE))
  expect_null(check_logical(na_lgl))
  expect_null(check_logical(lgl()))
  expect_null(check_logical(c(TRUE, FALSE, NA)))
  expect_null(check_logical(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_logical))
    err(checker(NULL, check_logical))
    err(checker(NA_integer_, check_logical))
    err(checker(1, check_logical))
    err(checker(list("foo", "bar"), check_logical, allow_null = TRUE))
    err(checker(NA, check_logical, allow_na = FALSE))
  })
})