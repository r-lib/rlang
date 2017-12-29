context("events")

test_that("can't add an exit event at top-level", {
  expect_true(TRUE)
  # This can only be tested interactively
  if (FALSE) {
    scoped_exit(1)  # Can't add an exit event at top level
  }
})

test_that("can add an exit event within a non-top-level global frame", {
  local(envir = global_env(), {
    `_x` <- list()
    rlang:::scoped_exit(`_x` <- c(`_x`, "bar"))
    `_x` <- c(`_x`, "foo")
  })

  expect_identical(env_get(global_env(), "_x"), list("foo", "bar"))
  env_unbind(global_env(), "_x")
})
