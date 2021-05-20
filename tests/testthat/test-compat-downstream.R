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
