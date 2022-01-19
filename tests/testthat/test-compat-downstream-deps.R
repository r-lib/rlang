test_that("can parse versions", {
  out <- .rlang_downstream_parse_deps(c("foo (>= 1.0)"))
  expect_equal(out, list(
    c(pkg = "foo", min = "1.0")
  ))

  out <- .rlang_downstream_parse_deps(c("foo (>= 1.0)", "bar (>= 2.0.0)"))
  expect_equal(out, list(
    c(pkg = "foo", min = "1.0"),
    c(pkg = "bar", min = "2.0.0")
  ))

  expect_error(
    .rlang_downstream_parse_deps("foo"),
    "Parsing error"
  )
  expect_error(
    .rlang_downstream_parse_deps("foo (1.0)"),
    "Parsing error"
  )
  expect_error(
    .rlang_downstream_parse_deps("foo (< 1.0)"),
    "Can only check `>=` requirements"
  )
})

test_that("can check downstream versions", {
  local_interactive(FALSE)

  ok_deps <- .rlang_downstream_parse_deps(c(
    "base (>= 1.0)",
    "utils (>= 1.1)"
  ))
  expect_no_warning(
    expect_true(
      .rlang_downstream_check(
        pkg = "rlang",
        pkg_ver = "0.5.0",
        deps = ok_deps,
        info = "Consequences.",
        env = env(checked = FALSE)
      )
    )
  )

  bad_deps <- .rlang_downstream_parse_deps(c(
    "base (>= 1.0)",
    "utils (>= 100.10)"
  ))
  expect_snapshot({
    (expect_warning({
      expect_false(
        .rlang_downstream_check(
          pkg = "rlang",
          pkg_ver = "0.5.0",
          deps = bad_deps,
          info = "Consequences.",
          env = env(checked = FALSE)
        )
      )
      NULL
    }))
  })

  missing_deps <- .rlang_downstream_parse_deps(c(
    "base (>= 1.0)",
    "foobar (>= 100.10)"
  ))
  expect_no_warning({
    expect_true(
      .rlang_downstream_check(
        pkg = "rlang",
        pkg_ver = "0.5.0",
        deps = missing_deps,
        info = "Consequences.",
        env = env(checked = FALSE)
      )
    )
    NULL
  })
})

test_that("setting `rlib_downstream_check` disables the check", {
  local_options(rlib_downstream_check = FALSE)
  local_interactive(FALSE)

  bad_deps <- .rlang_downstream_parse_deps(c(
    "base (>= 1.0)",
    "utils (>= 100.10)"
  ))
  expect_no_warning(
    expect_null(
      .rlang_downstream_check(
        pkg = "rlang",
        pkg_ver = "0.5.0",
        deps = bad_deps,
        info = "Consequences.",
        env = env(checked = FALSE)
      )
    )
  )
})

test_that("check_downstream() saves status in global env", {
  local_interactive(TRUE)
  local_options("rlang:::no_downstream_prompt" = TRUE)

  bad_deps <- .rlang_downstream_parse_deps(c(
    "base (>= 1.0)",
    "utils (>= 100.10)"
  ))

  key <- as.character(runif(1))
  check <- function() {
    .rlang_downstream_check(
      pkg = "rlang",
      pkg_ver = "0.5.0",
      deps = bad_deps,
      info = "Consequences.",
      deps_key = key,
      env = env(checked = FALSE)
    )
  }

  expect_warning(expect_false(check()))
  expect_no_warning(expect_null(check()))
})
