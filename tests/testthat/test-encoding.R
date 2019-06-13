context("encoding")

test_that("can roundtrip symbols in non-UTF8 locale", {
  with_non_utf8_locale({
    expect_identical(
      as_string(sym(get_alien_lang_string())),
      get_alien_lang_string()
    )
  })
})

test_that("Unicode escapes are always converted to UTF8 characters on roundtrip", {
  expect_identical(
    as_string(sym("<U+5E78><U+798F>")),
    "\u5E78\u798F"
  )
})

test_that("Unicode escapes are always converted to UTF8 characters with env_names()", {
  with_non_utf8_locale({
    env <- child_env(empty_env())
    env_bind(env, !! get_alien_lang_string() := NULL)
    expect_identical(env_names(env), get_alien_lang_string())
  })
})
