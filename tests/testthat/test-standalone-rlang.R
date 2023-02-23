test_that("is_installed() works", {
  for (is_installed in rlang_compats("is_installed")) {
    expect_true(is_installed("base"))
    expect_false(is_installed("_foo"))
  }
})

test_that("is_interactive() works", {
  for (is_interactive in rlang_compats("is_interactive")) {
    with_options(
      rlang_interactive = TRUE,
      expect_true(is_interactive())
    )
    with_options(
      rlang_interactive = FALSE,
      expect_false(is_interactive())
    )
  }
})

test_that("signallers work", {
  for (inform in rlang_compats("inform")) {
    expect_snapshot(inform(c("Header.", i = "Bullet.")))
  }
  for (warn in rlang_compats("warn")) {
    expect_snapshot(warn(c("Header.", i = "Bullet.")))
  }
  for (abort in rlang_compats("abort")) {
    expect_snapshot(abort(c("Header.", i = "Bullet.")), error = TRUE)
  }
})

test_that("unknown functions throw", {
  expect_snapshot(.rlang_compat("foo"), error = TRUE)
})
