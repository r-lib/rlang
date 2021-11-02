titles <- list(
  # Overviews
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

  # Guides
  topic_data_mask_ambiguity =
    "The data mask ambiguity",
  topic_double_evaluation =
    "The double evaluation problem",
  topic_multiple_columns =
    "Taking multiple columns without `...`",

  # Notes
  topic_embrace_non_args =
    "Does `{{` work on regular objects?",
  topic_embrace_constants =
    "Why are strings and other constants enquosed in the empty environment?",
  topic_inject_out_of_context =
    "What happens if I use injection operators out of context?"
)

# Can't use `{{` in links. Causes tex translation to crash.
link_title <- function(id) {
  switch(
    id,
    topic_data_mask = "What is data-masking and why do I need to embrace?",
    titles[[id]]
  )
}

sprintf_link <- function(id, topic = NULL) {
  # Link texts can't include code
  title <- gsub("`", "", link_title(id))
  title <- gsub("{", "\\{", title, fixed = TRUE)

  if (is.null(topic)) {
    topic <- gsub("_", "-", id)
  }

  sprintf("\\link[=%s]{%s}", topic, title)
}

links <- lapply(names(titles), sprintf_link)
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
