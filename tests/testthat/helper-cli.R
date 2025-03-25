cli_style <- with_options(
  cli.unicode = FALSE,
  cli_box_chars()
)

rlang_cli_local_support <- function(
  version,
  value = TRUE,
  frame = caller_env()
) {
  cache <- env_get(fn_env(.rlang_cli_has_cli), "cache")

  local_bindings(
    .env = cache,
    .frame = frame,
    "{version}" := value
  )
}

rlang_cli_local_hyperlinks <- function(frame = caller_env()) {
  local_options(
    cli.hyperlink = TRUE,
    .frame = frame
  )
  rlang_cli_local_support(
    CLI_SUPPORT_HYPERLINK_PARAMS,
    TRUE,
    frame = frame
  )
}
