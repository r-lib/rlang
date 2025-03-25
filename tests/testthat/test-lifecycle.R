test_that("deprecate_soft() warns when called from global env", {
  reset_warning_verbosity("rlang_test1")
  reset_warning_verbosity("rlang_test2")
  reset_warning_verbosity("rlang_test3")
  reset_warning_verbosity("rlang_test4")

  # Disable testthat handling
  withr::local_envvar(c("TESTTHAT_PKG" = ""))

  depr_soft <- function(id) deprecate_soft("foo", id)
  depr <- function(id) deprecate_warn("foo", id)

  # Indirect usage
  local_env <- env(
    ns_env("rlang"),
    depr_soft = depr_soft,
    depr = depr
  )
  local(envir = local_env, {
    expect_no_warning(depr_soft("rlang_test1"))
    expect_warning(depr("rlang_test2"), "foo")

    # Does not warn again
    expect_no_warning(depr("rlang_test2"))
  })

  # Direct usage
  local_bindings(
    .env = global_env(),
    depr_soft = depr_soft,
    depr = depr
  )
  local(envir = global_env(), {
    expect_warning(depr_soft("rlang_test3"), "foo")
    expect_warning(depr("rlang_test4"), "foo")

    # Warns again
    expect_warning(depr_soft("rlang_test3"), "foo")
    expect_warning(depr("rlang_test4"), "foo")
  })
})

test_that("deprecate_soft() warns when called from package being tested", {
  reset_warning_verbosity("rlang_test")

  withr::local_envvar(c("TESTTHAT_PKG" = "rlang"))
  depr <- function()
    deprecate_soft("warns from package being tested", id = "rlang_test")
  expect_warning(depr(), "warns from")
  expect_warning(depr(), "warns from")
})

test_that("deprecate_soft() indirect behaviour when warning verbosity is set", {
  reset_warning_verbosity("rlang_test")

  local_options(lifecycle_verbosity = "warning")

  local_env <- env(
    ns_env("base"),
    depr = inject(function(id) (!!deprecate_soft)("foo", id))
  )

  # FIXME: Is this a bug in lifecycle?
  local(envir = local_env, {
    expect_no_warning(depr("rlang_test"))
    expect_no_warning(depr("rlang_test"))
  })
})

test_that("can disable lifecycle warnings", {
  local_lifecycle_silence()
  expect_no_warning(deprecate_soft("foo"))
  expect_no_warning(deprecate_warn("foo"))
})

test_that("can promote lifecycle warnings to errors", {
  local_lifecycle_errors()
  expect_defunct(deprecate_soft("foo"), "foo")
  expect_defunct(deprecate_warn("foo"), "foo")
})

test_that("can enable warnings and errors with `with_` helpers", {
  expect_defunct(with_lifecycle_errors(deprecate_soft("foo")), "foo")
  expect_no_warning(with_lifecycle_warnings(with_lifecycle_silence(deprecate_warn(
    "foo"
  ))))

  # FIXME: Is this a bug in lifecycle?
  expect_no_warning(with_lifecycle_warnings(deprecate_soft("foo")))
})

test_that("soft-deprecation warnings are issued when called from child of global env as well", {
  fn <- function() deprecate_soft("called from child of global env")
  expect_warning(eval_bare(call2(fn), env(global_env())), "child of global env")
})

test_that("once-per-session note is not displayed on repeated warnings", {
  reset_warning_verbosity("once-per-session-note")

  wrn <- catch_cnd(
    deprecate_warn("foo", "once-per-session-note"),
    "lifecycle_warning_deprecated"
  )
  expect_true(grepl("once every", conditionMessage(wrn)))
})

test_that("lifecycle signallers support character vectors", {
  local_lifecycle_errors()
  expect_defunct(deprecate_soft(c("foo", "bar")), "foo\nbar")
  expect_defunct(deprecate_warn(c("foo", "bar")), "foo\nbar")
  expect_defunct(deprecate_stop(c("foo", "bar")), "foo\nbar")
})

test_that("the topenv of the empty env is not the global env", {
  expect_silent(deprecate_soft("topenv empty env", user_env = empty_env()))
})

test_that("can supply bullets", {
  expect_snapshot({
    deprecate_warn(c("foo", "i" = "bar"))
  })
})
