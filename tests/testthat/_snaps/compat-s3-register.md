# can register for generics that don't exist

    Code
      (expect_warning(s3_register("base::foobarbaz", "class", method = function(...)
        NULL)))
    Output
      <simpleWarning in register(): Can't find generic `foobarbaz` in package base to register S3 method.>

