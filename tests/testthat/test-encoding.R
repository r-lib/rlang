context("encoding")

test_that("can roundtrip symbols in non-UTF8 locale", {
  with_non_utf8_encoding({
    expect_identical(
      as_character(symbol(get_alien_lang_string())),
      get_alien_lang_string()
    )
  })
})

test_that("Unicode escapes are always converted to UTF8 characters on roundtrip", {
  expect_identical(
    as_character(symbol("<U+5E78><U+798F>")),
    "\u5E78\u798F"
  )
})

test_that("Unicode escapes are always converted to UTF8 characters in as_list()", {
  with_non_utf8_encoding({
    env <- child_env(empty_env())
    env_assign(env, get_alien_lang_string(), NULL)
    list <- as_list(env)
    expect_identical(names(list), get_alien_lang_string())
  })
})

test_that("Unicode escapes are always converted to UTF8 characters with names2()", {
  with_non_utf8_encoding({
    env <- child_env(empty_env())
    env_assign(env, get_alien_lang_string(), NULL)
    expect_identical(names2(env), get_alien_lang_string())
    list <- as_list(env)
    expect_identical(names2(list), get_alien_lang_string())
  })
})
