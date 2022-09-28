test_that("`check_bool()` checks", {
  expect_null(check_bool(TRUE))
  expect_null(check_bool(FALSE))
  expect_null(check_bool(NA, allow_na = TRUE))
  expect_null(check_bool(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(NA, check_bool))
    err(checker(NULL, check_bool))
    err(checker(lgl(), check_bool, allow_na = TRUE))
    err(checker(c(TRUE, FALSE), check_bool, allow_na = TRUE, allow_null = TRUE))
    err(checker(1, check_bool))
  })
})

test_that("`check_string()` checks", {
  expect_null(check_string(""))
  expect_null(check_string("foo"))
  expect_null(check_string(NA, allow_na = TRUE))
  expect_null(check_string(na_chr, allow_na = TRUE))
  expect_null(check_string(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker("", check_string, allow_empty = FALSE))
    err(checker(NA, check_string))
    err(checker(NULL, check_string))
    err(checker(chr(), check_string, allow_na = TRUE))
    err(checker(na_chr, check_string))
    err(checker(c("", ""), check_string, allow_na = TRUE, allow_null = TRUE))
    err(checker(1, check_string))
  })
})

test_that("`check_number()` checks", {
  expect_null(check_number(10))
  expect_null(check_number(10L))
  expect_null(check_number(NA, allow_na = TRUE))
  expect_null(check_number(na_dbl, allow_na = TRUE))
  expect_null(check_number(na_int, allow_na = TRUE))
  expect_null(check_number(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(NA, check_number))
    err(checker(NULL, check_number))
    err(checker(int(), check_number, allow_na = TRUE))
    err(checker(na_dbl, check_number))
    err(checker(na_int, check_number))
    err(checker(10:11, check_number, allow_na = TRUE, allow_null = TRUE))
    err(checker(10.5, check_number))
  })
})

test_that("`check_symbol()` checks", {
  expect_null(check_symbol(quote(foo)))
  expect_null(check_symbol(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(NULL, check_symbol))
    err(checker(TRUE, check_symbol, allow_na = TRUE))
    err(checker(alist(foo, bar), check_symbol, allow_na = TRUE, allow_null = TRUE))
    err(checker("foo", check_symbol))
    err(checker(quote(foo()), check_symbol))
  })
})

test_that("`check_call()` checks", {
  expect_null(check_call(quote(foo())))
  expect_null(check_call(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(NULL, check_call))
    err(checker(TRUE, check_call, allow_na = TRUE))
    err(checker(alist(foo(), bar()), check_call, allow_na = TRUE, allow_null = TRUE))
    err(checker(quote(foo), check_call))
  })
})

test_that("`check_environment()` checks", {
  expect_null(check_environment(env()))
  expect_null(check_environment(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(NULL, check_environment))
    err(checker(FALSE, check_environment, allow_na = TRUE))
    err(checker(list(env(), env()), check_environment, allow_na = TRUE, allow_null = TRUE))
  })
})

test_that("`check_character()` checks", {
  expect_null(check_character(""))
  expect_null(check_character(na_chr))
  expect_null(check_character(chr()))
  expect_null(check_character("foo"))
  expect_null(check_character(letters))
  expect_null(check_character(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(NULL, check_character))
    err(checker(NA, check_character))
    err(checker(1, check_character, allow_na = TRUE))
    err(checker(list("foo", "bar"), check_character, allow_na = TRUE, allow_null = TRUE))
  })
})
