options(
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

opt <- Sys.getenv("show_error_messages")
if (nzchar(opt)) {
  options(show.error.messages = as.logical(opt))
}

rlang::abort("Oh no")
