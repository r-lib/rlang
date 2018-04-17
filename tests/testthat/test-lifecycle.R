context("lifecycle")

test_that("can't use unregistered experimental function", {
  env <- env()
  with_options(topLevelEnvironment = env, {
    with_env(env, {
      expl <- function() {
        check_experimental("rlang::expl")
        "ok!"
      }
      wrapper <- function() expl()
      expect_error(wrapper(), "Please register it")

      register_experimental("rlang::expl")
      with_options(topLevelEnvironment = current_env(), {
        expect_identical(wrapper(), "ok!")
      })
    })
  })
})
