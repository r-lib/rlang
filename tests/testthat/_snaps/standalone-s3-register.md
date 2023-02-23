# can register for generics that don't exist

    Code
      (expect_warning(s3_register("testthat::foobarbaz", "class", method = function(
        ...) NULL)))
    Output
      <warning/rlang_warning>
      Warning:
      Can't find generic `foobarbaz` in package testthat to register S3 method.
      i This message is only shown to developers using devtools.
      i Do you need to update testthat to the latest version?

