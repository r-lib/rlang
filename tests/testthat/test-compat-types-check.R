test_that("`check_bool()` checks", {
  expect_null(check_bool(TRUE))
  expect_null(check_bool(FALSE))

  expect_snapshot({
    err(checker(NA, check_bool))
    err(checker(lgl(), check_bool))
    err(checker(c(TRUE, FALSE), check_bool))
    err(checker(1, check_bool))
  })
})

test_that("`check_string()` checks", {
  expect_null(check_string(""))
  expect_null(check_string("foo"))

  expect_snapshot({
    err(checker(NA, check_string))
    err(checker(chr(), check_string))
    err(checker(na_chr, check_string))
    err(checker(c("", ""), check_string))
    err(checker(1, check_string))
  })
})

test_that("`check_number()` checks", {
  expect_null(check_number(10))
  expect_null(check_number(10L))

  expect_snapshot({
    err(checker(NA, check_number))
    err(checker(int(), check_number))
    err(checker(na_dbl, check_number))
    err(checker(na_int, check_number))
    err(checker(10:11, check_number))
    err(checker(10.5, check_number))
  })
})

test_that("`check_symbol()` checks", {
  expect_null(check_symbol(quote(foo)))

  expect_snapshot({
    err(checker(TRUE, check_symbol))
    err(checker(alist(foo, bar), check_symbol))
    err(checker("foo", check_symbol))
    err(checker(quote(foo()), check_symbol))
  })
})

test_that("`check_call()` checks", {
  expect_null(check_call(quote(foo())))

  expect_snapshot({
    err(checker(TRUE, check_call))
    err(checker(alist(foo(), bar()), check_call))
    err(checker(quote(foo), check_call))
  })
})

test_that("`check_environment()` checks", {
  expect_null(check_environment(env()))

  expect_snapshot({
    err(checker(FALSE, check_environment))
    err(checker(list(env(), env()), check_environment))
  })
})

test_that("`check_character()` checks", {
  expect_null(check_character(""))
  expect_null(check_character(na_chr))
  expect_null(check_character(chr()))
  expect_null(check_character("foo"))
  expect_null(check_character(letters))

  expect_snapshot({
    err(checker(NA, check_character))
    err(checker(1, check_character))
    err(checker(list("foo", "bar"), check_character))
  })
})
