test_that("`check_bool()` checks", {
  expect_null(check_bool(TRUE))
  expect_null(check_bool(FALSE))
  expect_null(check_bool(NA, allow_na = TRUE))
  expect_null(check_bool(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_bool))
    err(checker(NA, check_bool))
    err(checker(NULL, check_bool))
    err(checker(lgl(), check_bool, allow_na = TRUE))
    err(checker(c(TRUE, FALSE), check_bool, allow_na = TRUE, allow_null = TRUE))
    err(checker(1, check_bool))
  })
})

test_that("`check_string()` checks", {
  expect_null(check_string("foo"))
  expect_null(check_string(""))
  expect_null(check_string(NA, allow_na = TRUE))
  expect_null(check_string(na_chr, allow_na = TRUE))
  expect_null(check_string(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker("", check_string, allow_empty = FALSE))
    err(checker(, check_string))
    err(checker(NA, check_string))
    err(checker(NULL, check_string))
    err(checker(chr(), check_string, allow_na = TRUE))
    err(checker(na_chr, check_string))
    err(checker(c("", ""), check_string, allow_na = TRUE, allow_null = TRUE))
    err(checker(1, check_string))
  })
})

test_that("`check_number_whole()` checks", {
  expect_null(check_number_whole(10))
  expect_null(check_number_whole(10L))
  expect_null(check_number_whole(NA, allow_na = TRUE))
  expect_null(check_number_whole(na_dbl, allow_na = TRUE))
  expect_null(check_number_whole(na_int, allow_na = TRUE))
  expect_null(check_number_whole(NULL, allow_null = TRUE))
  expect_null(check_number_whole(Inf, allow_infinite = TRUE))
  expect_null(check_number_whole(-Inf, allow_infinite = TRUE))

  check_number_whole(0, max = 0)
  check_number_whole(0, min = 0)
  check_number_whole(1, min = 0, max = 2)

  expect_snapshot({
    err(checker(, check_number_whole))
    err(checker(NA, check_number_whole))
    err(checker(NULL, check_number_whole))
    err(checker(int(), check_number_whole, allow_na = TRUE))
    err(checker(na_dbl, check_number_whole))
    err(checker(na_int, check_number_whole))
    err(checker(10:11, check_number_whole, allow_na = TRUE, allow_null = TRUE))
    err(checker(10.5, check_number_whole))
    err(checker(Inf, check_number_whole))
    err(checker(-Inf, check_number_whole))
    err(checker(1, max = 0, check_number_whole))
    err(checker(-1, min = 0, check_number_whole))
    err(checker(10, min = 1, max = 5, check_number_whole))
    err(checker(10, min = NA, check_number_whole))
    err(checker(10, min = NaN, check_number_whole))
    err(checker(10, max = NaN, check_number_whole))
  })
})

test_that("`check_number_decimal()` checks", {
  expect_null(check_number_decimal(10))
  expect_null(check_number_decimal(10L))
  expect_null(check_number_decimal(10.5))
  expect_null(check_number_decimal(NA, allow_na = TRUE))
  expect_null(check_number_decimal(na_dbl, allow_na = TRUE))
  expect_null(check_number_decimal(na_int, allow_na = TRUE))
  expect_null(check_number_decimal(NULL, allow_null = TRUE))
  expect_null(check_number_decimal(Inf))
  expect_null(check_number_decimal(-Inf))

  expect_snapshot({
    err(checker(, check_number_decimal))
    err(checker(NA, check_number_decimal))
    err(checker(NULL, check_number_decimal))
    err(checker(int(), check_number_decimal, allow_na = TRUE))
    err(checker(na_dbl, check_number_decimal))
    err(checker(na_int, check_number_decimal))
    err(checker(
      10:11,
      check_number_decimal,
      allow_na = TRUE,
      allow_null = TRUE
    ))
    err(checker(Inf, check_number_decimal, allow_infinite = FALSE))
    err(checker(-Inf, check_number_decimal, allow_infinite = FALSE))
    err(checker(10, min = NA, check_number_decimal))
    err(checker(10, min = NaN, check_number_decimal))
    err(checker(10, max = NaN, check_number_decimal))
  })
})

test_that("`check_symbol()` checks", {
  expect_null(check_symbol(quote(foo)))
  expect_null(check_symbol(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_symbol))
    err(checker(NULL, check_symbol))
    err(checker(TRUE, check_symbol))
    err(checker(alist(foo, bar), check_symbol, allow_null = TRUE))
    err(checker("foo", check_symbol))
    err(checker(quote(foo()), check_symbol))
  })
})

test_that("`check_call()` checks", {
  expect_null(check_call(quote(foo())))
  expect_null(check_call(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_call))
    err(checker(NULL, check_call))
    err(checker(TRUE, check_call))
    err(checker(alist(foo(), bar()), check_call, allow_null = TRUE))
    err(checker(quote(foo), check_call))
  })
})

test_that("`check_environment()` checks", {
  expect_null(check_environment(env()))
  expect_null(check_environment(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_environment))
    err(checker(NULL, check_environment))
    err(checker(FALSE, check_environment))
    err(checker(list(env(), env()), check_environment, allow_null = TRUE))
  })
})

test_that("`check_formula()` checks", {
  expect_null(check_formula(~foo))
  expect_null(check_formula(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_formula))
    err(checker(NULL, check_formula))
    err(checker(TRUE, check_formula))
    err(checker(quote(~foo), check_formula))
  })
})

test_that("`check_formula(allow_unevaluated = TRUE)` accepts unevaluated formulas", {
  # Unevaluated formula (no environment)
  f <- quote(~foo)

  # Default `allow_unevaluated = FALSE` rejects unevaluated formulas
  expect_error(check_formula(f), "evaluated formula")

  # `allow_unevaluated = TRUE` accepts any formula
  expect_null(check_formula(f, allow_unevaluated = TRUE))
  expect_null(check_formula(~foo, allow_unevaluated = TRUE))

  # Non-formulas are still rejected
  expect_snapshot({
    err(checker(TRUE, check_formula, allow_unevaluated = TRUE))
  })
})

test_that("non-numeric types are not numbers", {
  expect_snapshot({
    (expect_error(check_number_whole(factor("a"))))
    (expect_error(check_number_decimal(as.Date("2000-01-01"))))
  })
})

test_that("`check_data_frame()` checks", {
  expect_null(check_data_frame(data.frame()))
  expect_null(check_environment(NULL, allow_null = TRUE))

  expect_snapshot({
    err(checker(, check_data_frame))
    err(checker(NULL, check_data_frame))
    err(checker(
      list(data.frame(), data.frame()),
      check_data_frame,
      allow_null = TRUE
    ))
  })
})