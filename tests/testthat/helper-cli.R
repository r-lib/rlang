
cli_style <- cli_box_chars()

skip_unless_utf8 <- function() {
  skip_if(!cli_is_utf8_output())
}
