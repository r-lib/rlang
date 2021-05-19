# can request a line break in error bullets (#1130)

    Code
      (expect_error(abort(c("Main header.", "Header 1", x = "Bullet 1", x = "Bullet 2",
        "Header 2", x = "Bullet 3", x = "Bullet 4"))))
    Output
      <error/rlang_error>
      Main header.
      Header 1
      x Bullet 1
      x Bullet 2
      Header 2
      x Bullet 3
      x Bullet 4
    Code
      (expect_error(abort(c("Main header.", "Header 1", x = "Bullet 1", ` ` = "Break line",
        x = "Bullet 2", "", "Header 2", x = "Bullet 3", ` ` = "Break line", x = "Bullet 4")))
      )
    Output
      <error/rlang_error>
      Main header.
      Header 1
      x Bullet 1
        Break line
      x Bullet 2
      
      Header 2
      x Bullet 3
        Break line
      x Bullet 4

# format_error_bullets() generates bullets [plain]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\ni Bullet."

# format_error_bullets() generates bullets [ansi]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\n\033[34mi\033[39m Bullet."

# format_error_bullets() generates bullets [unicode]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\nℹ Bullet."

# format_error_bullets() generates bullets [fancy]

    Code
      format_error_bullets(c("Header.", i = "Bullet."))
    Output
      [1] "Header.\n\033[34mℹ\033[39m Bullet."

