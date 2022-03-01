titles <- list(
  # NSE

  ## Overviews
  topic_data_mask =
    "What is data-masking and why do I need `{{`?",
  topic_data_mask_programming =
    "Data mask programming patterns",
  topic_metaprogramming =
    "Metaprogramming patterns",
  topic_defuse =
    "Defusing R expressions",
  topic_inject =
    "Injecting with `!!`, `!!!`, and glue syntax",
  topic_quosure =
    "What are quosures and when are they needed?",

  ## Guides
  topic_data_mask_ambiguity =
    "The data mask ambiguity",
  topic_double_evaluation =
    "The double evaluation problem",
  topic_multiple_columns =
    "Taking multiple columns without `...`",

  ## Notes
  topic_embrace_non_args =
    "Does `{{` work on regular objects?",
  topic_embrace_constants =
    "Why are strings and other constants enquosed in the empty environment?",
  topic_inject_out_of_context =
    "What happens if I use injection operators out of context?",

  # Errors

  ## Guides
  topic_error_call =
    "Including function calls in error messages",
  topic_error_chaining =
    "Including contextual information with error chains",
  topic_condition_formatting =
    "Formatting messages with cli",

  ## Notes
  topic_condition_customisation =
    "Customising condition messages"
)

sprintf_topic_link <- function(id, topic = NULL) {
  if (is.null(topic)) {
    topic <- gsub("_", "-", id)
  }

  title <- titles[[id]]

  # Link texts can't include code
  html_title <- gsub("`", "", title)
  html_title <- gsub("{", "\\{", html_title, fixed = TRUE)
  html <- sprintf("\\link[=%s]{%s}", topic, html_title)

  # Link texts can't include curly symbols because the escpaing
  # routine of the Rd-to-TeX translators is broken
  text_title <- gsub("`", "", title)
  text_title <- gsub("{{", "curly-curly", text_title, fixed = TRUE)
  text_title <- gsub("{", "curly", text_title, fixed = TRUE)
  text <- sprintf("\\link[=%s]{%s}", topic, text_title)

  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}

links <- lapply(names(titles), sprintf_topic_link)
names(links) <- names(titles)

links[["{{"]] <- "\\ifelse{html}{\\code{\\link[=embrace-operator]{\\{\\{}}}{\\verb{\\{\\{}}"
links[["'{{'"]] <- "\\ifelse{html}{\\code{\\link[=glue-operators]{\"\\{\\{\"}}}{\\verb{\"\\{\\{\"}}"
links[["'{'"]] <- "\\ifelse{html}{\\code{\\link[=glue-operators]{\"\\{\"}}}{\\verb{\"\\{\"}}"

title <- function(id) {
  out <- titles[[id]]

  if (is.null(out)) {
    stop(sprintf("`id` '%s' doesn't exist.", id))
  }

  out
}
link <- function(id) {
  out <- links[[id]]

  if (is.null(out)) {
    stop(sprintf("`id` '%s' doesn't exist.", id))
  }

  out
}
text <- function(id) {
  switch(
    id,
    "'{'" = "\\verb{\"\\{\"}",
    "'{{'" = "\\verb{\"\\{\\{\"}",
    stop(sprintf("`id` '%s' doesn't exist.", id))
  )
}
