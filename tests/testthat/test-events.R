context("events")

test_that("can't add an exit event at top-level", {
  expect_error(local_exit(1, global_env()), "Can't add an exit event at top-level")
})

test_that("can add an exit event within a non-top-level global frame", {
  local(envir = global_env(), {
    `_x` <- TRUE
    rlang:::local_exit(`_x` <- FALSE)
  })

  expect_false(env_get(global_env(), "_x"))
  env_unbind(global_env(), "_x")
})
