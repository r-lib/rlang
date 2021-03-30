# can request a line break in error bullets (#1130)

    Code
      writeLines(format_error_bullets(c("Title", "foo bar", ` ` = "baz", "quux")))
    Output
      * Title
      * foo bar
        baz
      * quux

