# can register for generics that don't exist

    Code
      (expect_warning(s3_register("base::foobarbaz", "class", method = function(...)
        NULL)))
    Output
      <warning: Can't find generic `foobarbaz` in package base to register S3 method.
      i This message is only shown to developers using devtools.
      i Do you need to update base to the latest version?>

