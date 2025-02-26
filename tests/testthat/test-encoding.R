test_that("can roundtrip symbols in non-UTF8 locale", {
  skip_if_no_utf8_marker()
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
  skip_if_no_utf8_marker()
  with_non_utf8_locale({
    env <- child_env(empty_env())
    env_bind(env, !!get_alien_lang_string() := NULL)
    expect_identical(env_names(env), get_alien_lang_string())
  })
})

test_that("dots names are converted to and from UTF-8 (#1218)", {
  skip_if_not_windows()

  withr::local_locale(LC_CTYPE = "Chinese (Simplified)_China.936")
  x <- rawToChar(as.raw(c(0xb2, 0xe2, 0xca, 0xd4)))

  call <- list(quote(quos), 1)
  names(call)[[2]] <- x
  out <- eval(as.call(call))

  expect_equal(names(out), x)
})
